module GeniusYield.Types.Key
    ( -- * Payment verification key
      GYPaymentVerificationKey
    , paymentVerificationKeyFromApi
    , paymentVerificationKeyToApi
    , paymentVerificationKeyFromPlutus
    , paymentVerificationKeyToPlutus
    , pubKeyHash
      -- * Payment signing key
    , GYPaymentSigningKey
    , paymentSigningKeyFromApi
    , paymentSigningKeyToApi
    , readPaymentSigningKey
    , writePaymentSigningKey
    , paymentVerificationKey
    , generatePaymentSigningKey
) where

import           GeniusYield.Imports

import qualified Cardano.Api                  as Api
import qualified Data.Aeson.Types             as Aeson
import qualified Data.ByteString.Base16       as BS16
import qualified Data.ByteString.Char8        as BS8
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Ledger                       as Plutus
import qualified Plutus.V1.Ledger.Api         as Plutus
import qualified Plutus.V1.Ledger.Bytes       as Plutus
import qualified Text.Printf                  as Printf

import           GeniusYield.Types.PubKeyHash

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import           Data.Either                (isRight)
-- >>> import qualified Ledger                     as Plutus
-- >>> import qualified Text.Printf                as Printf

-------------------------------------------------------------------------------
-- Payment verification key (public)
-------------------------------------------------------------------------------

-- |
--
-- >>> "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYPaymentVerificationKey
-- GYPaymentVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
newtype GYPaymentVerificationKey = GYPaymentVerificationKey (Api.VerificationKey Api.PaymentKey)
    deriving stock Show
    deriving newtype (Eq, IsString)

-- |
--
-- >>> paymentVerificationKeyFromApi "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-- GYPaymentVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
paymentVerificationKeyFromApi :: Api.VerificationKey Api.PaymentKey -> GYPaymentVerificationKey
paymentVerificationKeyFromApi = coerce

-- |
--
-- >>> paymentVerificationKeyToApi "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-- "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
paymentVerificationKeyToApi :: GYPaymentVerificationKey -> Api.VerificationKey Api.PaymentKey
paymentVerificationKeyToApi = coerce

-- |
--
-- >>> paymentVerificationKeyFromPlutus (Plutus.PaymentPubKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605")
-- Just (GYPaymentVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605")
--
paymentVerificationKeyFromPlutus :: Plutus.PaymentPubKey -> Maybe GYPaymentVerificationKey
paymentVerificationKeyFromPlutus (Plutus.PaymentPubKey (Plutus.PubKey bs)) =
    GYPaymentVerificationKey <$> Api.deserialiseFromRawBytes (Api.AsVerificationKey Api.AsPaymentKey) (Plutus.bytes bs)

-- |
--
-- >>> paymentVerificationKeyToPlutus "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-- 0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605
--
paymentVerificationKeyToPlutus :: GYPaymentVerificationKey -> Plutus.PaymentPubKey
paymentVerificationKeyToPlutus = Plutus.PaymentPubKey . Plutus.PubKey . Plutus.fromBytes . Api.serialiseToRawBytes . paymentVerificationKeyToApi

pubKeyHash :: GYPaymentVerificationKey -> GYPubKeyHash
pubKeyHash = pubKeyHashFromApi . Api.verificationKeyHash . paymentVerificationKeyToApi

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode ("0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYPaymentVerificationKey)
-- "58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
instance Aeson.ToJSON GYPaymentVerificationKey where
    toJSON = Aeson.String . TE.decodeUtf8 . BS16.encode . Api.serialiseToCBOR . paymentVerificationKeyToApi

-- |
--
-- >>> Aeson.eitherDecode @GYPaymentVerificationKey "\"58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605\""
-- Right (GYPaymentVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605")
--
-- >>> Aeson.eitherDecode @GYPaymentVerificationKey "\"58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193zzz\""
-- Left "Error in $: invalid character at offset: 65"
--
instance Aeson.FromJSON GYPaymentVerificationKey where
    parseJSON (Aeson.String t) = case BS16.decode $ BS8.pack $ T.unpack t of
        Left err -> fail err
        Right bs -> case Api.deserialiseFromCBOR (Api.AsVerificationKey Api.AsPaymentKey) bs of
            Left err   -> fail $ show err
            Right skey -> return $ GYPaymentVerificationKey skey
    parseJSON _ = fail "payment verification key expected"

-- |
--
-- >>> Printf.printf "%s\n" ("0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYPaymentVerificationKey)
-- 0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605
--
instance Printf.PrintfArg GYPaymentVerificationKey where
    formatArg = Printf.formatArg . Api.serialiseToRawBytesHexText . paymentVerificationKeyToApi

-------------------------------------------------------------------------------
-- Payment signing key (private)
-------------------------------------------------------------------------------

-- |
--
-- >>> "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYPaymentSigningKey
-- GYPaymentSigningKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
--
newtype GYPaymentSigningKey = GYPaymentSigningKey (Api.SigningKey Api.PaymentKey)
    deriving stock Show
    deriving newtype IsString

instance Eq GYPaymentSigningKey where
    (==) = (==) `on` show

instance Ord GYPaymentSigningKey where
    compare = compare `on` show

-- |
--
-- >>> paymentSigningKeyFromApi "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-- GYPaymentSigningKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
--
paymentSigningKeyFromApi :: Api.SigningKey Api.PaymentKey -> GYPaymentSigningKey
paymentSigningKeyFromApi = coerce

-- |
--
-- >>> paymentSigningKeyToApi "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-- "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
--
paymentSigningKeyToApi :: GYPaymentSigningKey -> Api.SigningKey Api.PaymentKey
paymentSigningKeyToApi = coerce

-- | Reads a payment signing key from a file.
--
readPaymentSigningKey :: FilePath -> IO GYPaymentSigningKey
readPaymentSigningKey fp = do
    s <- Api.readFileTextEnvelope (Api.AsSigningKey Api.AsPaymentKey) fp
    case s of
        Left err -> fail (show err) --- throws IOError
        Right x  -> return (GYPaymentSigningKey x)

-- | Writes a payment signing key to a file.
--
writePaymentSigningKey :: FilePath -> GYPaymentSigningKey -> IO ()
writePaymentSigningKey file key = do
    e <- Api.writeFileTextEnvelope file (Just "Payment Signing Key") $ paymentSigningKeyToApi key
    case e of
        Left (err :: Api.FileError ()) -> throwIO $ userError $ show err
        Right ()                       -> return ()

-- |
--
-- >>> paymentVerificationKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-- GYPaymentVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
paymentVerificationKey :: GYPaymentSigningKey -> GYPaymentVerificationKey
paymentVerificationKey = GYPaymentVerificationKey . Api.getVerificationKey . paymentSigningKeyToApi

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode ("5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYPaymentSigningKey)
-- "58205ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
--
instance Aeson.ToJSON GYPaymentSigningKey where
    toJSON = Aeson.String . TE.decodeUtf8 . BS16.encode . Api.serialiseToCBOR . paymentSigningKeyToApi

-- |
--
-- >>> Aeson.eitherDecode @GYPaymentSigningKey "\"58205ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290\""
-- Right (GYPaymentSigningKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290")
--
-- >>> Aeson.eitherDecode @GYPaymentSigningKey "\"58205ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fceczzz\""
-- Left "Error in $: invalid character at offset: 65"
--
instance Aeson.FromJSON GYPaymentSigningKey where
    parseJSON (Aeson.String t) = case BS16.decode $ BS8.pack $ T.unpack t of
        Left err -> fail err
        Right bs -> case Api.deserialiseFromCBOR (Api.AsSigningKey Api.AsPaymentKey) bs of
            Left err   -> fail $ show err
            Right skey -> return $ GYPaymentSigningKey skey
    parseJSON _ = fail "payment signing key expected"

-- |
--
-- >>> Printf.printf "%s\n" ("5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYPaymentSigningKey)
-- 5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290
--
instance Printf.PrintfArg GYPaymentSigningKey where
    formatArg = Printf.formatArg . Api.serialiseToRawBytesHexText . paymentSigningKeyToApi

-- | Generates a new random payment signing key.
--
generatePaymentSigningKey :: IO GYPaymentSigningKey
generatePaymentSigningKey = paymentSigningKeyFromApi <$> Api.generateSigningKey Api.AsPaymentKey
