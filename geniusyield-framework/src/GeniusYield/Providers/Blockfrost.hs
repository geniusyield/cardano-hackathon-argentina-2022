module GeniusYield.Providers.Blockfrost
    ( Blockfrost.Project
    , blockfrostProtocolParams
    , blockfrostStakePools
    , blockfrostSystemStart
    , blockfrostEraHistory
    , blockfrostQueryUtxo
    , blockfrostLookupDatum
    , blockfrostSlotActions
    , blockfrostSubmitTx
    ) where

import           Control.Exception            (Exception, throwIO)
import           Control.Monad                ((<=<))
import           Control.Monad.Except         (throwError)
import           Data.Bifunctor               (first)
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy         as LBS
import           Data.Functor                 ((<&>))
import           Data.List                    (find)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (fromJust)
import           Data.Proxy                   (Proxy (Proxy))
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.String                  (fromString)
import           Data.Text                    (Text)
import qualified Data.Text                    as Txt

import           Codec.Serialise              (deserialise)
import qualified Data.Aeson                   as Aeson
import qualified Data.Time.Clock.POSIX        as Time
import qualified Money

import qualified Blockfrost.Client            as Blockfrost
import qualified Cardano.Api                  as Api
import qualified Cardano.Api.Shelley          as Api.S
import           Cardano.Slotting.Time
import           PlutusTx                     (Data (..), FromData, fromData)
import           PlutusTx.Builtins            as Plutus

import           GeniusYield.Providers.Common
import           GeniusYield.Types

data BlockfrostProviderException
    = BlpvApiError Text Blockfrost.BlockfrostError
    | BlpvDeserializeFailure Text SomeDeserializeError -- ^ This error should never actually happen (unless there's a bug).
    | BlpvNoSlotInfo Blockfrost.BlockHash
    | BlpvUnsupportedOperation Text
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

handleBlockfrostError :: Text -> Either Blockfrost.BlockfrostError a -> IO a
handleBlockfrostError locationInfo = either (throwIO . BlpvApiError locationInfo) pure

scriptDataToData :: Api.ScriptData -> Data
scriptDataToData (Api.ScriptDataConstructor n xs) = Constr n $ scriptDataToData <$> xs
scriptDataToData (Api.ScriptDataMap xs)           = Map [(scriptDataToData x, scriptDataToData y) | (x, y) <- xs]
scriptDataToData (Api.ScriptDataList xs)          = List $ scriptDataToData <$> xs
scriptDataToData (Api.ScriptDataNumber n)         = I n
scriptDataToData (Api.ScriptDataBytes bs)         = B bs

lovelacesToInteger :: Blockfrost.Lovelaces -> Integer
lovelacesToInteger = fromIntegral

gyAddressToBlockfrost :: GYAddress -> Blockfrost.Address
gyAddressToBlockfrost = Blockfrost.mkAddress . addressToText

amountToValue :: Blockfrost.Amount -> GYValue
amountToValue (Blockfrost.AdaAmount lovelaces) = valueSingleton GYLovelace $ lovelacesToInteger lovelaces
amountToValue (Blockfrost.AssetAmount sdiscr) = valueSingleton (GYToken cs tkName) $ Money.someDiscreteAmount sdiscr
  where
    csAndTkname = Money.someDiscreteCurrency sdiscr
    (fromString . Txt.unpack -> cs, tokenNameFromHexUnsafe -> tkName) = Txt.splitAt 56 csAndTkname

fromJson :: FromData a => LBS.ByteString -> Either SomeDeserializeError a
fromJson b = do
    v <- first DeserializeErrorAeson $ Aeson.eitherDecode b
    x <- first DeserializeErrorScriptDataJson $ Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema v
    pure . fromJust . fromData $ scriptDataToData x

-------------------------------------------------------------------------------
-- Submit
-------------------------------------------------------------------------------

blockfrostSubmitTx :: Blockfrost.Project -> GYSubmitTx
blockfrostSubmitTx proj tx = do
    txId <- handleBlockfrostError "SubmitTx" <=< Blockfrost.runBlockfrost proj
        . Blockfrost.submitTx
        . Blockfrost.CBORString
        . LBS.fromStrict
        . Api.serialiseToCBOR
        $ txToApi tx
    pure . fromString . Txt.unpack $ Blockfrost.unTxHash txId

-------------------------------------------------------------------------------
-- Slot actions
-------------------------------------------------------------------------------

blockfrostSlotActions :: Blockfrost.Project -> GYSlotActions
blockfrostSlotActions proj = GYSlotActions
    { gyGetCurrentSlot'   = getCurrentSlot
    , gyWaitForNextBlock' = gyWaitForNextBlockDefault getCurrentSlot
    , gyWaitUntilSlot'    = gyWaitUntilSlotDefault getCurrentSlot
    }
  where
    getCurrentSlot = blockfrostGetCurrentSlot proj

blockfrostGetCurrentSlot :: Blockfrost.Project -> IO GYSlot
blockfrostGetCurrentSlot proj = do
    Blockfrost.Block {_blockSlot=slotMaybe, _blockHash=hash} <-
        Blockfrost.runBlockfrost proj Blockfrost.getLatestBlock >>= handleBlockfrostError "Slot"
    case slotMaybe of
        Nothing -> throwIO $ BlpvNoSlotInfo hash
        Just x  -> pure . slotFromApi . Api.SlotNo . fromInteger $ Blockfrost.unSlot x

-------------------------------------------------------------------------------
-- Query UTxO
-------------------------------------------------------------------------------

blockfrostQueryUtxo :: Blockfrost.Project -> GYQueryUTxO
blockfrostQueryUtxo proj = GYQueryUTxO
    { gyQueryUtxosAtAddress'   = blockfrostUtxosAtAddress proj
    , gyQueryUtxosAtTxOutRefs' = blockfrostUtxosAtTxOutRefs proj
    , gyQueryUtxoAtTxOutRef'   = blockfrostUtxosAtTxOutRef proj
    }

blockfrostUtxosAtAddress :: Blockfrost.Project -> GYAddress -> IO GYUTxOs
blockfrostUtxosAtAddress proj addr = do
    {- 'Blockfrost.getAddressUtxos' doesn't return all utxos at that address, only the first 100 or so.
    Have to handle paging manually for all. -}
    addrUtxos <- handleBlockfrostError "AddressUtxos" <=< Blockfrost.runBlockfrost proj
        . Blockfrost.allPages $ \paged ->
            Blockfrost.getAddressUtxos' (gyAddressToBlockfrost addr) paged Blockfrost.Ascending
    pure . utxosFromList $ map transformUtxo addrUtxos
  where
    transformUtxo Blockfrost.AddressUtxo {..} = GYUTxO
        { utxoRef       = fromString $ Txt.unpack (Blockfrost.unTxHash _addressUtxoTxHash) ++ '#' : show _addressUtxoOutputIndex
        , utxoAddress   = addr
        , utxoValue     = foldMap amountToValue _addressUtxoAmount
        , utxoDatumHash = fromString . Txt.unpack . Blockfrost.unDatumHash <$> _addressUtxoDataHash
        }

blockfrostUtxosAtTxOutRef :: Blockfrost.Project -> GYTxOutRef -> IO (Maybe GYUTxO)
blockfrostUtxosAtTxOutRef proj ref = do
    let (Api.serialiseToRawBytesHexText -> txId, utxoIdx) = txOutRefToTuple ref
    -- Get all UTxO outputs created by the tx id within the given tx out ref.
    txOutMaybe <- handler
        <=< Blockfrost.runBlockfrost proj . Blockfrost.getTxUtxos $ Blockfrost.TxHash txId
    pure $ txOutMaybe >>= \(Blockfrost._transactionUtxosOutputs -> outs) ->
        (\Blockfrost.UtxoOutput {..} -> GYUTxO
            { utxoRef       = ref
            , utxoAddress   = addressFromTextUnsafe $ Blockfrost.unAddress _utxoOutputAddress
            , utxoValue     = foldMap amountToValue _utxoOutputAmount
            , utxoDatumHash = fromString . Txt.unpack . Blockfrost.unDatumHash <$> _utxoOutputDataHash
            }
        )
        -- Find the utxo output with the idx within the given tx out ref.
        <$> find (\(Blockfrost._utxoOutputOutputIndex -> idx) -> idx == utxoIdx) outs
  where
     -- This particular error is fine in this case, we can just return 'Nothing'.
    handler (Left Blockfrost.BlockfrostNotFound) = pure Nothing
    handler other                                = handleBlockfrostError "TxUtxos(single)" $ Just <$> other

blockfrostUtxosAtTxOutRefs :: Blockfrost.Project -> [GYTxOutRef] -> IO GYUTxOs
blockfrostUtxosAtTxOutRefs proj refs = do
    {- This combines utxo refs with the same tx id, yielding a 'Map Api.TxId (Set Integer)'.

    That is, a map from transaction hash to a set of utxo indices within that transaction,
    that the caller is interested in.
    -}
    let refMap =
            Map.fromListWith (<>)
            $ map ((\(!txId, !utxoIdx) -> (txId, Set.singleton utxoIdx)) . txOutRefToTuple) refs
    {- For each tx id, query blockfrost for the utxo outputs produced by said tx.

    Once all the outputs are obtained, filter to only end up with the utxo indices the caller
    is interested in.
    -}
    txUtxoMap <- handleBlockfrostError "TxUtxos" <=< Blockfrost.runBlockfrost proj . flip Map.traverseWithKey refMap
        $ \txId idxs -> do
            res <- Blockfrost.tryError
                $ Blockfrost.getTxUtxos . Blockfrost.TxHash $ Api.serialiseToRawBytesHexText txId
            case res of
                Left Blockfrost.BlockfrostNotFound -> pure []
                Left err                             -> throwError err
                Right (Blockfrost._transactionUtxosOutputs -> outs) -> pure $
                    filter (\(Blockfrost._utxoOutputOutputIndex -> idx) -> idx `Set.member` idxs) outs
    -- Create a 'GYUTxOs' map from the 'Map Api.TxId [Blockfrost.UtxoOutput]'
    pure
        . utxosFromList
        . concat
        $ Map.foldlWithKey'
            (\acc txId outs -> map (transformUtxo txId) outs : acc)
            []
            txUtxoMap
  where
    transformUtxo txId Blockfrost.UtxoOutput {..} =
        GYUTxO
            { utxoRef       = txOutRefFromApi . Api.TxIn txId . Api.TxIx $ fromInteger _utxoOutputOutputIndex
            , utxoAddress   = addressFromTextUnsafe $ Blockfrost.unAddress _utxoOutputAddress
            , utxoValue     = foldMap amountToValue _utxoOutputAmount
            , utxoDatumHash = fromString . Txt.unpack . Blockfrost.unDatumHash <$> _utxoOutputDataHash
            }

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

blockfrostProtocolParams :: Blockfrost.Project -> IO Api.S.ProtocolParameters
blockfrostProtocolParams proj = do
    Blockfrost.ProtocolParams {..} <- Blockfrost.runBlockfrost proj Blockfrost.getLatestEpochProtocolParams
        >>= handleBlockfrostError "ProtocolParams"
    pure $ Api.S.ProtocolParameters
        { protocolParamProtocolVersion     = (fromInteger _protocolParamsProtocolMajorVer, fromInteger _protocolParamsProtocolMinorVer)
        , protocolParamDecentralization    = Just _protocolParamsDecentralisationParam
        -- TODO: Blockfrost gives back a 'Maybe Aeson.Value' in extra_entropy. Usable?
        , protocolParamExtraPraosEntropy   = Nothing
        , protocolParamMaxBlockHeaderSize  = fromInteger _protocolParamsMaxBlockHeaderSize
        , protocolParamMaxBlockBodySize    = fromInteger _protocolParamsMaxBlockSize
        , protocolParamMaxTxSize           = fromInteger _protocolParamsMaxTxSize
        , protocolParamTxFeeFixed          = fromInteger _protocolParamsMinFeeB
        , protocolParamTxFeePerByte        = fromInteger _protocolParamsMinFeeA
        , protocolParamMinUTxOValue        = Just . Api.Lovelace $ lovelacesToInteger _protocolParamsMinUtxo
        , protocolParamStakeAddressDeposit = Api.Lovelace $ lovelacesToInteger _protocolParamsKeyDeposit
        , protocolParamStakePoolDeposit    = Api.Lovelace $ lovelacesToInteger _protocolParamsPoolDeposit
        , protocolParamMinPoolCost         = Api.Lovelace $ lovelacesToInteger _protocolParamsMinPoolCost
        , protocolParamPoolRetireMaxEpoch  = Api.EpochNo $ fromInteger _protocolParamsEMax
        , protocolParamStakePoolTargetNum  = fromInteger _protocolParamsNOpt
        , protocolParamPoolPledgeInfluence = _protocolParamsA0
        , protocolParamMonetaryExpansion   = _protocolParamsRho
        , protocolParamTreasuryCut         = _protocolParamsTau
        , protocolParamUTxOCostPerWord     = Just . Api.Lovelace $ lovelacesToInteger _protocolParamsCoinsPerUtxoWord
        , protocolParamPrices              = Just $ Api.S.ExecutionUnitPrices _protocolParamsPriceStep _protocolParamsPriceMem
        , protocolParamMaxTxExUnits        = Just $ Api.ExecutionUnits (fromInteger $ Blockfrost.unQuantity _protocolParamsMaxTxExSteps) (fromInteger $ Blockfrost.unQuantity _protocolParamsMaxTxExMem)
        , protocolParamMaxBlockExUnits     = Just $ Api.ExecutionUnits (fromInteger $ Blockfrost.unQuantity _protocolParamsMaxBlockExSteps) (fromInteger $ Blockfrost.unQuantity _protocolParamsMaxBlockExMem)
        , protocolParamMaxValueSize        = Just . fromInteger . Blockfrost.unQuantity $ _protocolParamsMaxValSize
        , protocolParamCollateralPercent   = Just $ fromInteger _protocolParamsCollateralPercent
        , protocolParamMaxCollateralInputs = Just $ fromInteger _protocolParamsMaxCollateralInputs
        , protocolParamCostModels          = toApiCostModel _protocolParamsCostModels
        , protocolParamUTxOCostPerByte     = Nothing
        }
  where
    toApiCostModel = Map.fromList
        . Map.foldlWithKey (\acc k x -> case k of
            Blockfrost.PlutusV1 -> (Api.S.AnyPlutusScriptVersion Api.PlutusScriptV1, Api.CostModel x) : acc
            Blockfrost.PlutusV2 -> (Api.S.AnyPlutusScriptVersion Api.PlutusScriptV2, Api.CostModel x) : acc
            -- Don't care about non plutus cost models.
            Blockfrost.Timelock -> acc
        )
        []
        . Blockfrost.unCostModels

blockfrostStakePools :: Blockfrost.Project -> IO (Set Api.S.PoolId)
blockfrostStakePools proj = do
    {- 'Blockfrost.listPools' doesn't actually return all pools, only the first 100 or so.
    Have to handle paging manually for all. -}
    stkPools <- handleBlockfrostError locationIdent <=< Blockfrost.runBlockfrost proj
        . Blockfrost.allPages $ \paged -> Blockfrost.listPools' paged Blockfrost.Ascending
    -- The pool ids returned by blockfrost are in bech32.
    let poolIdsEith = traverse
            (Api.deserialiseFromBech32 (Api.proxyToAsType $ Proxy @Api.S.PoolId) . Blockfrost.unPoolId)
            stkPools
    case poolIdsEith of
        -- Deserialization failure shouldn't happen on blockfrost returned pool id.
        Left err -> throwIO . BlpvDeserializeFailure locationIdent $ DeserializeErrorBech32 err
        Right has -> pure $ Set.fromList has
  where
    locationIdent = "ListPools"

blockfrostSystemStart :: Blockfrost.Project -> IO SystemStart
blockfrostSystemStart proj = do
  genesisParams <- Blockfrost.runBlockfrost proj Blockfrost.getLedgerGenesis >>= handleBlockfrostError "LedgerGenesis"
  pure . SystemStart . Time.posixSecondsToUTCTime $ Blockfrost._genesisSystemStart genesisParams

-- Seems like blockfrost doesn't have an endpoint for this? Use hard coded preprod one.
blockfrostEraHistory :: Blockfrost.Project -> IO (Api.EraHistory Api.CardanoMode)
blockfrostEraHistory Blockfrost.Project {projectEnv = Blockfrost.Preprod} = do
    -- Use the pre-provided hard coded era history interpreter file that supports the preprod testnet.
    Api.S.EraHistory Api.CardanoMode . deserialise <$> BSL.readFile "testnet/era_hist_interpreter"
blockfrostEraHistory _ = throwIO $ BlpvUnsupportedOperation "EraHistory"

-------------------------------------------------------------------------------
-- Datum lookup
-------------------------------------------------------------------------------

blockfrostLookupDatum :: Blockfrost.Project -> GYLookupDatum
blockfrostLookupDatum p dh = do
    datumMaybe <- handler <=< Blockfrost.runBlockfrost p
        . Blockfrost.getScriptDatum . Blockfrost.DatumHash . Txt.pack . show $ datumHashToPlutus dh
    sequence $ datumMaybe <&> \(Blockfrost.ScriptDatum v) -> case fromJson @Plutus.BuiltinData (Aeson.encode v) of
      Left err -> throwIO $ BlpvDeserializeFailure locationIdent err
      Right bd -> pure $ GYDatum bd
  where
    -- This particular error is fine in this case, we can just return 'Nothing'.
    handler (Left Blockfrost.BlockfrostNotFound) = pure Nothing
    handler other                                = handleBlockfrostError locationIdent $ Just <$> other
    locationIdent = "LookupDatum"
