{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Providers using the Maestro blockchain API.
module GeniusYield.Providers.Maestro
    ( maestroSubmitTx
    , maestroSlotActions
    , maestroQueryUtxo
    , maestroLookupDatum
    , maestroProtocolParams
    , maestroEraHistory
    , maestroSystemStart
    , maestroStakePools
    , newMaestroApiEnv
    ) where

import           Control.Exception               (Exception, throwIO)
import           Control.Monad.Except            (MonadError (catchError, throwError),
                                                  (<=<))
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as BSL
import           Data.Functor                    ((<&>))
import           Data.Maybe                      (catMaybes)
import           Data.Proxy                      (Proxy (Proxy))
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.String                     (fromString)
import           Data.Text                       (Text)
import qualified Data.Text                       as Txt
import           Data.Time                       (LocalTime)
import qualified Data.Time                       as Time
import           Data.Traversable                (for)
import           Data.Word                       (Word64)
import           GHC.Generics                    (Generic)

import           Codec.Serialise                 (deserialise)
import           Data.Aeson                      (FromJSON (parseJSON))
import qualified Data.Aeson                      as Aeson
import           Data.Aeson.TH                   (Options (fieldLabelModifier),
                                                  defaultOptions,
                                                  deriveFromJSON)
import qualified Data.Aeson.Types                as Aeson
import           Network.HTTP.Types.Status       (status404)
import           Servant.API                     (Capture, Get, Header', JSON,
                                                  Required, Strict,
                                                  type (:<|>) (..), type (:>))
import           Servant.Client                  (ClientEnv,
                                                  ClientError (FailureResponse),
                                                  ClientM,
                                                  ResponseF (Response, responseStatusCode),
                                                  client, runClientM)

import qualified Cardano.Api                     as Api
import qualified Cardano.Api.Shelley             as Api.S
import qualified Cardano.Slotting.Time           as C

import           GeniusYield.Types
import           GeniusYield.Utils               (fieldNamePrefixStrip2,
                                                  fieldNamePrefixStrip3)

import           GeniusYield.Providers.Common
import           GeniusYield.Providers.SubmitApi

type HexStringOf a = Text

type Bech32StringOf a = Text

newtype ScriptDataDetailed = ScriptDataDetailed Api.ScriptData deriving stock (Eq, Show)

instance FromJSON ScriptDataDetailed where
    parseJSON = either (Aeson.parseFail . show) (pure . ScriptDataDetailed)
        . Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema

newtype MaestroAssetClass = MaestroAssetClass GYAssetClass deriving stock (Eq, Show)

instance FromJSON MaestroAssetClass where
    parseJSON = Aeson.withText "MaestroAssetClass" $ \case
        "lovelace" -> pure $ MaestroAssetClass GYLovelace
        csAndTkname -> case Txt.splitOn "#" csAndTkname of
            [fromString . Txt.unpack -> cs, tokenNameFromHex -> tkNameMaybe] -> case tkNameMaybe of
                Left err -> Aeson.parseFail $ "Failed parsing token name: " ++ show err
                Right tkName -> pure . MaestroAssetClass $ GYToken cs tkName
            _ -> Aeson.parseFail "Failed parsing asset class: expected policy id and hex encoded token name separated by '#'"

data MaestroAsset = MaestroAsset { maQuantity :: Word, maUnit :: MaestroAssetClass }
    deriving stock (Eq, Show)

-- | { datum: string, datum_hash: string, index: int, tx_hash: string, assets: [MaestroAsset] }
data MaestroUtxo = MaestroUtxo
    { _muDatum    :: Maybe Text -- ^ Inline datum, ignore for now. FIXME when framework has support.
    , muDatumHash :: Maybe (HexStringOf GYDatumHash)
    , muIndex     :: Word
    , muTxHash    :: HexStringOf GYTxId
    , muAssets    :: [MaestroAsset]
    , muAddress   :: Bech32StringOf GYAddress
    }

-- | { number: int }
newtype MaestroSlotNo = MaestroSlotNo { msnNumber :: Word64 }
    deriving stock (Eq, Ord, Show, Generic)

-- | { time: string }
newtype MaestroSystemStart = MaestroSystemStart { mssTime :: LocalTime }
    deriving stock (Eq, Ord, Show, Generic)

-- | { bytes: string; json: object }
data MaestroDatum = MaestroDatum
    { mdBytes :: HexStringOf GYDatum -- ^ CBOR of the datum.
    , mdJson  :: ScriptDataDetailed   -- ^ 'ScriptData' JSON in 'ScriptDataJsonDetailedSchema' format.
    }
    deriving stock Show

type MaestroApi
    = Header' '[Required, Strict] "X-API-Token" Int :> "addresses"
        :> Capture "addr" (Bech32StringOf GYAddress) :> "utxos" :> Get '[JSON] [MaestroUtxo]
    :<|> Header' '[Required, Strict] "X-API-Token" Int
        :> "current-slot" :> Get '[JSON] MaestroSlotNo
    :<|> Header' '[Required, Strict] "X-API-Token" Int :> "datum"
        :> Capture "datumHash" (HexStringOf GYDatumHash) :> Get '[JSON] MaestroDatum
    :<|> Header' '[Required, Strict] "X-API-Token" Int :> "transactions"
        :> Capture "txHash" (HexStringOf GYTxId) :> "outputs"
        :> Capture "utxoIdx" Integer :> "utxo" :> Get '[JSON] MaestroUtxo
    :<|> Header' '[Required, Strict] "X-API-Token" Int :> "system-start" :> Get '[JSON] MaestroSystemStart
    :<|> Header' '[Required, Strict] "X-API-Token" Int
        :> "stake-pools" :> Get '[JSON] [Bech32StringOf Api.S.PoolId]
    :<|> Header' '[Required, Strict] "X-API-Token" Int :> TxSubmitApi

getUtxosAtAddress :: Int -> Bech32StringOf GYAddress -> ClientM [MaestroUtxo]
getUtxoByRef :: Int -> HexStringOf GYTxId -> Integer -> ClientM MaestroUtxo
findDatumByHash :: Int -> HexStringOf GYDatumHash -> ClientM MaestroDatum
getCurrentSlot :: Int -> ClientM MaestroSlotNo
getSystemStart :: Int -> ClientM MaestroSystemStart
getStakePools :: Int -> ClientM [Bech32StringOf Api.S.PoolId]
txSubmitPost :: Int -> ByteString -> ClientM Api.S.TxId

getUtxosAtAddress
    :<|> getCurrentSlot
    :<|> findDatumByHash
    :<|> getUtxoByRef
    :<|> getSystemStart
    :<|> getStakePools
    :<|> txSubmitPost
     = client @MaestroApi Proxy

data MaestroProviderException
    = MspvApiError Text ClientError
    | MspvDeserializeFailure Text SomeDeserializeError
    | MspvMultiUtxoPerRef GYTxOutRef -- ^ The API returned several utxos for a single TxOutRef.
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

-- | The Maestro blockchain API env.
data MaestroApiEnv = MaestroApiEnv ClientEnv Int

newMaestroApiEnv :: String -> Int -> IO MaestroApiEnv
newMaestroApiEnv baseUrl apiToken = flip MaestroApiEnv apiToken <$> newServantClientEnv baseUrl

runMaestroClient :: (Int -> ClientM a) -> MaestroApiEnv -> IO (Either ClientError a)
runMaestroClient x (MaestroApiEnv cEnv apiToken) = runClientM (x apiToken) cEnv

handleMaestroError :: Text -> Either ClientError a -> IO a
handleMaestroError locationInfo = either (throwIO . MspvApiError locationInfo) pure

assetToValue :: MaestroAsset -> GYValue
assetToValue MaestroAsset{maQuantity, maUnit=MaestroAssetClass asc} = valueSingleton asc $ toInteger maQuantity

-------------------------------------------------------------------------------
-- Submit
-------------------------------------------------------------------------------

maestroSubmitTx :: MaestroApiEnv -> GYSubmitTx
maestroSubmitTx (MaestroApiEnv env apiToken) = txSubmitPost apiToken `submitApiSubmitTxCore` env

-------------------------------------------------------------------------------
-- Slot actions
-------------------------------------------------------------------------------

maestroSlotActions :: MaestroApiEnv -> GYSlotActions
maestroSlotActions env = GYSlotActions
    { gyGetCurrentSlot'   = x
    , gyWaitForNextBlock' = gyWaitForNextBlockDefault x
    , gyWaitUntilSlot'    = gyWaitUntilSlotDefault x
    }
  where
    x = maestroGetCurrentSlot env

maestroGetCurrentSlot :: MaestroApiEnv -> IO GYSlot
maestroGetCurrentSlot env =
    runMaestroClient getCurrentSlot env >>= handleMaestroError "CurrentSlot" <&> slotFromApi . Api.SlotNo . msnNumber

-------------------------------------------------------------------------------
-- Query UTxO
-------------------------------------------------------------------------------

maestroQueryUtxo :: MaestroApiEnv -> GYQueryUTxO
maestroQueryUtxo env = GYQueryUTxO
    { gyQueryUtxosAtAddress'   = maestroUtxosAtAddress env
    , gyQueryUtxosAtTxOutRefs' = maestroUtxosAtTxOutRefs env
    , gyQueryUtxoAtTxOutRef'   = maestroUtxosAtTxOutRef env
    }

maestroUtxosAtAddress :: MaestroApiEnv -> GYAddress -> IO GYUTxOs
maestroUtxosAtAddress env addr = do
    addrUtxos <- flip getUtxosAtAddress (addressToText addr) `runMaestroClient` env >>= handleMaestroError "AddressUtxos"
    pure . utxosFromList $ map transformUtxo addrUtxos
  where
    transformUtxo MaestroUtxo {muDatumHash, muIndex, muTxHash, muAssets} = GYUTxO
        { utxoRef       = fromString $ Txt.unpack muTxHash ++ '#' : show muIndex
        , utxoAddress   = addr
        , utxoValue     = foldMap assetToValue muAssets
        , utxoDatumHash = fromString . Txt.unpack <$> muDatumHash
        }

maestroUtxosAtTxOutRef :: MaestroApiEnv -> GYTxOutRef -> IO (Maybe GYUTxO)
maestroUtxosAtTxOutRef env ref = do
    res <- maestroUtxosAtTxOutRefs' env [ref]
    case res of
        []  -> pure Nothing
        [x] -> pure $ Just x
        -- This shouldn't happen.
        _   -> throwIO $ MspvMultiUtxoPerRef ref

maestroUtxosAtTxOutRefs :: MaestroApiEnv -> [GYTxOutRef] -> IO GYUTxOs
maestroUtxosAtTxOutRefs env = fmap utxosFromList . maestroUtxosAtTxOutRefs' env

maestroUtxosAtTxOutRefs' :: MaestroApiEnv -> [GYTxOutRef] -> IO [GYUTxO]
maestroUtxosAtTxOutRefs' env refs = do
    res <- handleMaestroError "UtxoByRefs" <=< flip runMaestroClient env $ \apiToken -> for refs $ \ref -> do
        let (Api.serialiseToRawBytesHexText -> txId, utxoIdx) = txOutRefToTuple ref
        (Just <$> getUtxoByRef apiToken txId utxoIdx) `catchError` handler
    pure $ f <$> catMaybes res
  where
    f MaestroUtxo {muDatumHash, muIndex, muTxHash, muAssets, muAddress} = GYUTxO
        { utxoRef       = fromString $ Txt.unpack muTxHash ++ '#' : show muIndex
        , utxoAddress   = addressFromTextUnsafe muAddress
        , utxoValue     = foldMap assetToValue muAssets
        , utxoDatumHash = fromString . Txt.unpack <$> muDatumHash
        }
    handler (FailureResponse _ Response{responseStatusCode})
        | responseStatusCode == status404 = pure Nothing
    handler other = throwError other

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- We just read the hard coded protocol params for preprod currently.
maestroProtocolParams :: IO Api.S.ProtocolParameters
maestroProtocolParams = do
    res <- Aeson.eitherDecodeFileStrict' "testnet/protocol-params.json"
    either (throwIO . MspvDeserializeFailure "ProtocolParams" . DeserializeErrorAeson) pure res

maestroStakePools :: MaestroApiEnv -> IO (Set Api.S.PoolId)
maestroStakePools env = do
    stkPools <- runMaestroClient getStakePools env >>= handleMaestroError locationIdent
    -- The pool ids returned by blockfrost are in bech32.
    let poolIdsEith = traverse
            (Api.deserialiseFromBech32 (Api.proxyToAsType $ Proxy @Api.S.PoolId))
            stkPools
    case poolIdsEith of
        -- Deserialization failure shouldn't happen on blockfrost returned pool id.
        Left err -> throwIO . MspvDeserializeFailure locationIdent $ DeserializeErrorBech32 err
        Right has -> pure $ Set.fromList has
  where
    locationIdent = "ListPools"

maestroSystemStart :: MaestroApiEnv -> IO C.SystemStart
maestroSystemStart = fmap (C.SystemStart . Time.localTimeToUTC Time.utc . mssTime) . handleMaestroError "SystemStart"
    <=< runMaestroClient getSystemStart

-- Use hard coded preprod one.
maestroEraHistory :: IO (Api.EraHistory Api.CardanoMode)
maestroEraHistory =
    -- Use the pre-provided hard coded era history interpreter file that supports the preprod testnet.
    Api.S.EraHistory Api.CardanoMode . deserialise <$> BSL.readFile "testnet/era_hist_interpreter"

-------------------------------------------------------------------------------
-- Datum lookup
-------------------------------------------------------------------------------

maestroLookupDatum :: MaestroApiEnv -> GYLookupDatum
maestroLookupDatum env dh = do
    datumMaybe <- handler <=< flip runMaestroClient env
        . flip findDatumByHash . Txt.pack . show $ datumHashToPlutus dh
    pure $ datumMaybe <&> \(MaestroDatum _ (ScriptDataDetailed scriptData)) -> datumFromApi' scriptData
  where
    -- This particular error is fine in this case, we can just return 'Nothing'.
    handler (Left (FailureResponse _ Response{responseStatusCode}))
        | responseStatusCode == status404 = pure Nothing
    handler other = handleMaestroError "LookupDatum" $ Just <$> other

-------------------------------------------------------------------------------
-- Template Haskell Boilerplate
-------------------------------------------------------------------------------

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = fieldNamePrefixStrip3
      }
    ''MaestroSlotNo
 )

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = fieldNamePrefixStrip3
      }
    ''MaestroSystemStart
 )

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = fieldNamePrefixStrip2
      }
    ''MaestroAsset
 )

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = fieldNamePrefixStrip2
      }
    ''MaestroDatum
 )

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \x -> case fieldNamePrefixStrip2 x of
            "datumHash" -> "datum_hash"
            "txHash"    -> "tx_hash"
            a           -> a

      }
    ''MaestroUtxo
 )
