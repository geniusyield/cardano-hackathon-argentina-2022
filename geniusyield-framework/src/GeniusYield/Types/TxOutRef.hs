module GeniusYield.Types.TxOutRef (
    GYTxOutRef,
    txOutRefToPlutus,
    txOutRefFromPlutus,
    txOutRefFromApi,
    txOutRefToApi,
    -- * Helpers
    showTxOutRef,
    txOutRefToTuple,
    -- * CBOR format
    GYTxOutRefCbor (..),
) where

import           Data.Hashable                    (Hashable (..))
import           GeniusYield.Imports
import           Control.Lens                     ((?~), (&))

import qualified Cardano.Api                      as Api
import qualified Codec.CBOR.Read                  as CBOR
import qualified Codec.CBOR.Term                  as CBOR
import qualified Data.Aeson                       as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Base16           as Base16
import qualified Data.ByteString.Encoding         as BSE
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Csv                         as Csv
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import qualified Ledger                           as Plutus
import qualified Ledger.Tx.CardanoAPI             as Conv
import qualified Text.Printf                      as Printf
import qualified Web.HttpApiData                  as Web
import qualified Data.Swagger                     as Swagger
import qualified Data.Swagger.Internal.Schema     as Swagger
import qualified Data.Swagger.Lens                ()

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Csv                   as Csv
-- >>> import qualified Web.HttpApiData            as Web
--

-------------------------------------------------------------------------------
-- GYTxOutRef
-------------------------------------------------------------------------------

newtype GYTxOutRef = GYTxOutRef Api.TxIn
  deriving         (Show, Eq, Ord)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

instance Hashable GYTxOutRef where
    hashWithSalt salt (GYTxOutRef (Api.TxIn x (Api.TxIx y))) = salt
        `hashWithSalt` Api.serialiseToRawBytes x
        `hashWithSalt` y

txOutRefFromPlutus :: Plutus.TxOutRef -> Maybe GYTxOutRef
txOutRefFromPlutus r = case Conv.toCardanoTxIn r of
    Left _  -> Nothing
    Right x -> Just (GYTxOutRef x)

txOutRefToPlutus :: GYTxOutRef -> Plutus.TxOutRef
txOutRefToPlutus = coerce Conv.fromCardanoTxIn

txOutRefFromApi :: Api.TxIn -> GYTxOutRef
txOutRefFromApi = coerce

txOutRefToApi :: GYTxOutRef -> Api.TxIn
txOutRefToApi = coerce

txOutRefToTuple :: GYTxOutRef -> (Api.TxId, Integer)
txOutRefToTuple (GYTxOutRef (Api.TxIn x (Api.TxIx y))) = (x, toInteger y)

-- |
--
-- >>> Web.parseUrlPiece @GYTxOutRef "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1"
-- Right (GYTxOutRef (TxIn "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" (TxIx 1)))
--
instance Web.FromHttpApiData GYTxOutRef where
    -- copy parseTxIn from cardano-api
    parseUrlPiece tr = case Atto.parseOnly parser (TE.encodeUtf8 tr) of
        Left err -> Left (T.pack ("GYTxOutRef: " ++ err))
        Right x  -> Right x
      where
        parser :: Atto.Parser GYTxOutRef
        parser = do
            tx  <- Base16.decodeLenient <$> Atto.takeWhile1 isHexDigit
            _   <- Atto.char '#'
            ix  <- Atto.decimal
            tx' <- maybe (fail $ "not txid bytes: " ++ show tx) return $ Api.deserialiseFromRawBytes Api.AsTxId tx
            return (GYTxOutRef (Api.TxIn tx' (Api.TxIx ix)))

instance Web.ToHttpApiData GYTxOutRef where
    toUrlPiece oref = showTxOutRef oref

instance Printf.PrintfArg GYTxOutRef where
    formatArg oref ff = Printf.formatArg (showTxOutRef oref) ff

-- renderTxIn in cardano-api
showTxOutRef :: GYTxOutRef -> Text
showTxOutRef (GYTxOutRef (Api.TxIn txId (Api.TxIx ix))) =
    Api.serialiseToRawBytesHexText txId <> "#" <> T.pack (show ix)

-- |
--
-- >>> "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1" :: GYTxOutRef
-- GYTxOutRef (TxIn "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" (TxIx 1))
--
-- >>> "not-a-tx-out-ref" :: GYTxOutRef
-- *** Exception: invalid GYTxOutRef: not-a-tx-out-ref
-- ...
--
instance IsString GYTxOutRef where
    fromString s = fromRight (error $ "invalid GYTxOutRef: " <> s) $ Web.parseUrlPiece $ T.pack s

-- |
--
-- >>> Csv.toField ("4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1" :: GYTxOutRef)
-- "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1"
--
instance Csv.ToField GYTxOutRef where
    toField = BSE.encode BSE.char8 . showTxOutRef

-- |
--
-- >>> Csv.runParser $ Csv.parseField @GYTxOutRef "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1"
-- Right (GYTxOutRef (TxIn "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" (TxIx 1)))
--
-- >>> Csv.runParser $ Csv.parseField @GYTxOutRef "not a tx-out ref"
-- Left "GYTxOutRef: Failed reading: takeWhile1"
--
instance Csv.FromField GYTxOutRef where
    parseField = either (fail . T.unpack) return . Web.parseUrlPiece . BSE.decode BSE.char8

-------------------------------------------------------------------------------
-- swagger schema
-------------------------------------------------------------------------------

instance Swagger.ToParamSchema GYTxOutRef where
  toParamSchema _ = mempty
                  & Swagger.type_ ?~ Swagger.SwaggerString
                  & Swagger.format ?~ "hex"
                  & Swagger.pattern ?~ "[0-9a-fA-F]{64}#\"d+"

instance Swagger.ToSchema GYTxOutRef where
  declareNamedSchema _ = pure $ Swagger.named "GYTxOutRef" $ Swagger.paramSchemaToSchema (Proxy @GYTxOutRef)
                       & Swagger.example ?~ toJSON ("4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1" :: Text)

-------------------------------------------------------------------------------
-- GYTxOutRefCbor
-------------------------------------------------------------------------------

newtype GYTxOutRefCbor = GYTxOutRefCbor { getTxOutRefHex :: GYTxOutRef }

-- |
--
-- >>> Web.parseUrlPiece @GYTxOutRefCbor "8282582004ffecdf5f3ced5c5c788833415bcbef26e3e21290fcebcf8216327e21569e420082583900e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f6161a004c4b40"
-- Right GYTxOutRef (TxIn "04ffecdf5f3ced5c5c788833415bcbef26e3e21290fcebcf8216327e21569e42" (TxIx 0))
--
-- >>> Web.parseUrlPiece @GYTxOutRefCbor "00"
-- Left "Invalid TxIn CBOR structure"
--
instance Web.FromHttpApiData GYTxOutRefCbor where
  parseUrlPiece t = do
      bs           <- first T.pack $ Base16.decode $ TE.encodeUtf8 t
      (rest, cbor) <- first (T.pack . show) $ CBOR.deserialiseFromBytes CBOR.decodeTerm $ LBS.fromStrict bs
      unless (LBS.null rest) $ Left "Left overs in input"
      case cbor of
          CBOR.TList [CBOR.TList [CBOR.TBytes tx, CBOR.TInt ix], _] -> do
              tx' <- maybe (Left $ T.pack $ "not txid bytes: " ++ show tx) return $ Api.deserialiseFromRawBytes Api.AsTxId tx
              unless (ix >= 0) $ Left "negative ix"
              return (GYTxOutRefCbor (GYTxOutRef (Api.TxIn tx' (Api.TxIx (fromIntegral ix)))))
          _ -> Left "Invalid TxIn CBOR structure"

instance Show GYTxOutRefCbor where
  show (GYTxOutRefCbor tx) = show tx

instance Printf.PrintfArg GYTxOutRefCbor where
    formatArg (GYTxOutRefCbor (GYTxOutRef txRef)) ff = Printf.formatArg (show txRef) ff

instance Aeson.FromJSON GYTxOutRefCbor where
    parseJSON v = do
        t <- Aeson.parseJSON v
        case Web.parseUrlPiece t of
            Left err  -> fail $ T.unpack err
            Right ref -> return ref

-------------------------------------------------------------------------------
-- swagger schema
-------------------------------------------------------------------------------

instance Swagger.ToParamSchema GYTxOutRefCbor where
  toParamSchema _ = mempty
                  & Swagger.type_ ?~ Swagger.SwaggerString
                  & Swagger.format ?~ "cbor hex"
                  & Swagger.pattern ?~ "[0-9a-fA-F]+"

instance Swagger.ToSchema GYTxOutRefCbor where
  declareNamedSchema p = Swagger.plain $ Swagger.paramSchemaToSchema p
                       & Swagger.example ?~ toJSON ("8282582004ffecdf5f3ced5c5c788833415bcbef26e3e21290fcebcf8216327e21569e420082583900e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f6161a004c4b40" :: Text)
