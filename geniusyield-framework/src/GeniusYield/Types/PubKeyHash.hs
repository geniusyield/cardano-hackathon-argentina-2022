module GeniusYield.Types.PubKeyHash (
    GYPubKeyHash,
    pubKeyHashFromPlutus,
    pubKeyHashToPlutus,
    pubKeyHashToApi,
    pubKeyHashFromApi,
) where

import           GeniusYield.Imports
import           Control.Lens                    ((&), (?~))

import qualified Cardano.Api                  as Api
import qualified Data.Aeson.Types             as Aeson
import qualified Ledger                       as Plutus
import qualified Ledger.Tx.CardanoAPI         as Conv
import qualified Plutus.V1.Ledger.Api         as Plutus
import qualified Text.Printf                  as Printf
import qualified Data.Csv                     as Csv
import qualified Data.Swagger                 as Swagger
import qualified Data.Swagger.Internal.Schema as Swagger

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import qualified Data.Csv                   as Csv
-- >>> import qualified Text.Printf                as Printf

newtype GYPubKeyHash = GYPubKeyHash (Api.Hash Api.PaymentKey)
    deriving stock Show
    deriving newtype (Eq, Ord, IsString)

-- |
--
-- >>> pubKeyHashFromPlutus "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-- Right (GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")
--
-- >>> pubKeyHashFromPlutus "abcd"
-- Left "Tag \"toCardanoPaymentKeyHash (2 bytes)\" DeserialisationError"
--
pubKeyHashFromPlutus :: Plutus.PubKeyHash -> Either String GYPubKeyHash
pubKeyHashFromPlutus h = case Conv.toCardanoPaymentKeyHash (Plutus.PaymentPubKeyHash h) of
    Left err -> Left (show err)
    Right x -> Right (GYPubKeyHash x)

-- |
--
-- >>> let Just pkh = Aeson.decode @GYPubKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
-- >>> pubKeyHashToPlutus pkh
-- e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d
--
pubKeyHashToPlutus :: GYPubKeyHash -> Plutus.PubKeyHash
pubKeyHashToPlutus = coerce fromCardanoPaymentKeyHash where
    -- this is not exported from plutus-ledger
    fromCardanoPaymentKeyHash :: Api.Hash Api.PaymentKey -> Plutus.PubKeyHash
    fromCardanoPaymentKeyHash paymentKeyHash = Plutus.PubKeyHash $ Plutus.toBuiltin $ Api.serialiseToRawBytes paymentKeyHash

-- |
--
-- >>> let Just pkh = Aeson.decode @GYPubKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
-- >>> pubKeyHashToApi pkh
-- "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
--
pubKeyHashToApi :: GYPubKeyHash -> Api.Hash Api.PaymentKey
pubKeyHashToApi = coerce

-- |
--
-- >>> pubKeyHashFromApi "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-- GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
--
pubKeyHashFromApi :: Api.Hash Api.PaymentKey -> GYPubKeyHash
pubKeyHashFromApi = coerce

-- |
--
-- >>> let Just pkh = Aeson.decode @GYPubKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
-- >>> LBS8.putStrLn $ Aeson.encode pkh
-- "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
--
instance Aeson.ToJSON GYPubKeyHash where
    toJSON = Aeson.toJSON . Api.serialiseToRawBytesHexText . pubKeyHashToApi

-- |
--
-- >>> Aeson.eitherDecode @GYPubKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
-- Right (GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")
--
-- Invalid characters:
--
-- >>> Aeson.eitherDecode @GYPubKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6azzz\""
-- Left "Error in $.getPubKeyHash: invalid character at offset: 53"
--
instance Aeson.FromJSON GYPubKeyHash where
    parseJSON v = do
        h <- Aeson.parseJSON (Aeson.Object $ "getPubKeyHash" Aeson..= v)
        either fail return (pubKeyHashFromPlutus h)

-- |
--
-- >>> Printf.printf "%s\n" $ pubKeyHashFromApi "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-- e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d
--
instance Printf.PrintfArg GYPubKeyHash where
    formatArg = Printf.formatArg . Api.serialiseToRawBytesHexText . pubKeyHashToApi

-- |
--
-- >>> Csv.toField @GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-- "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
--
instance Csv.ToField GYPubKeyHash where
    toField = Api.serialiseToRawBytesHex . pubKeyHashToApi

-- |
--
-- >>> Csv.runParser $ Csv.parseField @GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-- Right (GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")
--
-- >>> Csv.runParser $ Csv.parseField @GYPubKeyHash "not a pubkey hash"
-- Left "RawBytesHexErrorBase16DecodeFail \"not a pubkey hash\" \"invalid bytestring size\""
--
instance Csv.FromField GYPubKeyHash where
    parseField = either (fail . show) (return . pubKeyHashFromApi) . Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsPaymentKey)

-------------------------------------------------------------------------------
-- swagger schema
-------------------------------------------------------------------------------


instance Swagger.ToSchema GYPubKeyHash where
  declareNamedSchema _ = pure $ Swagger.named "GYAddress" $ mempty
                       & Swagger.type_           ?~ Swagger.SwaggerString
                       & Swagger.format          ?~ "hex"
                       & Swagger.description     ?~ "The hash of a public key."
                       & Swagger.example         ?~ toJSON ("e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d" :: Text)
                       & Swagger.maxLength       ?~ 56
                       & Swagger.minLength ?~ 56
