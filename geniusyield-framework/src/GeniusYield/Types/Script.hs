module GeniusYield.Types.Script (
    -- * Validator
    GYValidator,
    validatorFromPlutus,
    validatorToPlutus,
    validatorToApi,
    validatorFromApi,
    writeValidator,
    readValidator,
    validatorToCborHex,
    validatorFromCborHex,

    -- ** Selectors
    validatorHash,
    validatorPlutusHash,
    validatorApiHash,

    -- * ValidatorHash
    GYValidatorHash,
    validatorHashToApi,
    validatorHashToPlutus,
    validatorHashFromApi,

    -- * MintingPolicy
    GYMintingPolicy,
    mintingPolicyId,
    mintingPolicyFromPlutus,
    mintingPolicyToPlutus,
    mintingPolicyToApi,
    mintingPolicyFromApi,
    writeMintingPolicy,
    mintingPolicyToCborHex,
    mintingPolicyFromCborHex,

    -- ** Selectors
    mintingPolicyCurrencySymbol,
    mintingPolicyApiId,

    -- * MintingPolicyId
    GYMintingPolicyId,
    mintingPolicyIdToApi,
    mintingPolicyIdFromApi,
    mintingPolicyIdCurrencySymbol,
) where

import qualified Cardano.Api                      as Api
import qualified Cardano.Api.Shelley              as Api.S
import qualified Codec.Serialise
import           Control.Lens                        ((&), (?~))
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.ByteString.Short            as SBS
import qualified Data.Swagger                     as Swagger
import qualified Data.Swagger.Internal.Schema     as Swagger
import qualified Data.Text.Encoding               as TE
import qualified Data.Text                        as T
import qualified Ledger.Scripts
import qualified Ledger.Tx.CardanoAPI             as Conv
import qualified Plutus.V1.Ledger.Api             as Plutus
import qualified Plutus.V1.Ledger.Scripts         as Plutus
import qualified Text.Printf                      as Printf
import qualified Web.HttpApiData                  as Web

import           GeniusYield.Imports

-- $setup
--
-- >>> import GeniusYield.Imports

-------------------------------------------------------------------------------
-- Validator
-------------------------------------------------------------------------------

newtype GYValidator = GYValidator GYScript
  deriving (Eq, Ord, Show)

validatorFromPlutus :: Plutus.Validator -> GYValidator
validatorFromPlutus = coerce scriptFromPlutus

validatorToPlutus :: GYValidator -> Plutus.Validator
validatorToPlutus = coerce scriptToPlutus

validatorToApi :: GYValidator -> Api.PlutusScript Api.PlutusScriptV1
validatorToApi = coerce scriptToApi

validatorFromApi :: Api.PlutusScript Api.PlutusScriptV1 -> GYValidator
validatorFromApi = coerce scriptFromApi

validatorHash :: GYValidator -> GYValidatorHash
validatorHash = coerce scriptApiHash

validatorPlutusHash :: GYValidator -> Plutus.ValidatorHash
validatorPlutusHash = coerce scriptPlutusHash

validatorApiHash :: GYValidator -> Api.ScriptHash
validatorApiHash = coerce scriptApiHash

-- | Writes a validator to a file.
--
writeValidator :: FilePath -> GYValidator -> IO ()
writeValidator file v = do
    e <- Api.writeFileTextEnvelope file (Just "Validator") $ validatorToApi v
    case e of
        Left (err :: Api.FileError ()) -> throwIO $ userError $ show err
        Right ()                       -> return ()

readValidator :: FilePath -> IO GYValidator
readValidator file = do
    e <- Api.readFileTextEnvelope (Api.AsPlutusScript Api.AsPlutusScriptV1) file
    case e of
        Left (err :: Api.FileError Api.TextEnvelopeError) -> throwIO $ userError $ show err
        Right s                                           -> return $ validatorFromApi s

validatorToCborHex :: GYValidator -> BS.ByteString
validatorToCborHex = Api.serialiseToRawBytesHex . validatorToApi

-- |
--
-- >>> validatorFromCborHex "5902c1010000323232323232323232323232322223232323232533300f3014300e00213232323253330133018301200213232325333016333013222533301700213301423301401e3756603a002002264a6660300042a6602c6602800203c26602a46602a6eacc07800407cc07000c4c8c8c94ccc074cdc78010008a9980c9980b802001899803980f803180f8028a99980e99b90002001153301933017004021133007301f006005153301933017021003133007006301f005375c6036603a0086eb8c068c070010dd5980e980d8011bab301c301a0023756602e60300146602644a66602c0022038264a666032a6602a66ebcc068c06cc06c0040104cdd7980d00080289bab301a301b301c3754603400426006603600460366ea8c064004dd6180b8038a4c2c66e9520003357406602444a66602a0022c264a66603066ebcc064c0680040144c0640044c00cc068008c068dd5180c0009bac301630170054bd70180a804180a002899299980a1980891299980a0008a50153330163375e602e00200629444c008c060004dd6180a8020a4c2c6ea4dd7180a004180a8009baa009301230123012301230123012001301130123754602000a2c60220026ea8c038c03c00cc038004c038dd500218069baa00122333005222533300900213300623012375a601e002002264a6660140042a6601066e2400520001330072337126eb4c0400052000300e0031323232533300f3371e0040022a6601666e2401000c4cc01cc044018c04401454ccc03ccdc80010008a9980599b89004480004cc01cc04401801454cc02cc05400c4cc01c018c044014dd7180698078021bae300c300e004375a601e601a0046eb4c038c0300080080048c00c894ccc0180045288a9980218019804800898011805000911998030010008a502323002233002002001230022330020020015573eaae755cd2ba15744aae792f5bded8c06e1d20023712900001"
-- Just (GYValidator (GYScript "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0"))
--
validatorFromCborHex :: BS.ByteString -> Maybe GYValidator
validatorFromCborHex = either (const Nothing) (Just . validatorFromApi)
                     . Api.deserialiseFromRawBytesHex (Api.AsPlutusScript Api.AsPlutusScriptV1)

newtype GYValidatorHash = GYValidatorHash Api.ScriptHash
  deriving stock (Show, Eq, Ord)

-- |
--
-- >>> "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0" :: GYValidatorHash
-- GYValidatorHash "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0"
--
instance IsString GYValidatorHash where
    fromString = GYValidatorHash . fromString

-- |
--
-- >>> printf "%s" ("cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0" :: GYValidatorHash)
-- cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0
--
instance Printf.PrintfArg GYValidatorHash where
    formatArg (GYValidatorHash h) = formatArg $ init $ tail $ show h

validatorHashToPlutus :: GYValidatorHash -> Plutus.ValidatorHash
validatorHashToPlutus (GYValidatorHash h) =
    Plutus.ValidatorHash $ Plutus.toBuiltin $ Api.serialiseToRawBytes h

validatorHashToApi :: GYValidatorHash -> Api.ScriptHash
validatorHashToApi = coerce

validatorHashFromApi :: Api.ScriptHash -> GYValidatorHash
validatorHashFromApi = coerce

-------------------------------------------------------------------------------
-- Minting Policy
-------------------------------------------------------------------------------

newtype GYMintingPolicy = GYMintingPolicy GYScript
  deriving (Eq, Ord, Show)

mintingPolicyId :: GYMintingPolicy -> GYMintingPolicyId
mintingPolicyId = coerce scriptApiHash

mintingPolicyFromPlutus :: Plutus.MintingPolicy -> GYMintingPolicy
mintingPolicyFromPlutus = coerce scriptFromPlutus

mintingPolicyToPlutus :: GYMintingPolicy -> Plutus.MintingPolicy
mintingPolicyToPlutus = coerce scriptToPlutus

mintingPolicyToApi :: GYMintingPolicy -> Api.PlutusScript Api.PlutusScriptV1
mintingPolicyToApi = coerce scriptToApi

mintingPolicyFromApi :: Api.PlutusScript Api.PlutusScriptV1 -> GYMintingPolicy
mintingPolicyFromApi = coerce scriptFromApi

mintingPolicyCurrencySymbol :: GYMintingPolicy -> Plutus.CurrencySymbol
mintingPolicyCurrencySymbol = coerce scriptPlutusHash

mintingPolicyApiId :: GYMintingPolicy -> Api.PolicyId
mintingPolicyApiId = coerce scriptApiHash

-- | Writes a minting policy to a file.
--
writeMintingPolicy :: FilePath -> GYMintingPolicy -> IO ()
writeMintingPolicy file p = do
    e <- Api.writeFileTextEnvelope file (Just "Minting Policy") $ mintingPolicyToApi p
    case e of
        Left (err :: Api.FileError ()) -> throwIO $ userError $ show err
        Right ()                       -> return ()

mintingPolicyToCborHex :: GYMintingPolicy -> BS.ByteString
mintingPolicyToCborHex = Api.serialiseToRawBytesHex . mintingPolicyToApi

-- |
--
-- >>> mintingPolicyFromCborHex "59018401000032323232323232323232323232223232533300e301130090021533300e3300a23371290001bad30110013333008300600337566018600e60206ea8c030c040dd500190008b0b0a4c26464646464a6660280022c2a666026a660146601e466ebc014c048c058dd50009bac30113015375400a2a660146ae8cc04800454cc028c058dd6980a9808800899b8f375c601c60220020042930b199980618050039bab3010300b3014375400840022c6e48cdc59bad300f3010001375c601e60266ea8c03c004c048dd50009806801180618081baa003300f00137540044466601600400229408c8c94ccc02ccdc3a4000600c00426eb8c02400458c030004dd51803980418059baa001230073007300700123223300622533300c00112250011533300b3375e600c60120020082644460046eacc04000cc0240044c008c028004004dd4800aab9d2300222533300800114a02a66600e6006600a00229444c008c0180048c8c0088cc0080080048c0088cc0080080055d0aba25734aae7d55cf1b8748009"
-- Just (GYMintingPolicy (GYScript "d6abee1cd55a4591b8cc812d25fc0e51e7eced956d06db5abbdaa5d7"))
--
mintingPolicyFromCborHex :: BS.ByteString -> Maybe GYMintingPolicy
mintingPolicyFromCborHex = either (const Nothing) (Just . mintingPolicyFromApi)
                         . Api.deserialiseFromRawBytesHex (Api.AsPlutusScript Api.AsPlutusScriptV1)


-- | Minting policy identifier, also a currency symbol.
newtype GYMintingPolicyId = GYMintingPolicyId Api.PolicyId
  deriving stock   (Eq, Ord)
  deriving newtype (Generic, ToJSON, FromJSON)

-- |
--
-- >>> fromString "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" :: GYMintingPolicyId
-- "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef"
--
instance IsString GYMintingPolicyId where
    fromString = GYMintingPolicyId . fromString

instance Show GYMintingPolicyId where
    showsPrec d (GYMintingPolicyId s) = showsPrec d s

instance Web.FromHttpApiData GYMintingPolicyId where
  parseUrlPiece = first T.pack . Atto.parseOnly parser . TE.encodeUtf8
    where
      parser :: Atto.Parser GYMintingPolicyId
      parser  = do
        cs <- Atto.takeWhile1 isHexDigit

        case Api.deserialiseFromRawBytesHex Api.AsPolicyId cs of
          Left x    -> fail $ "Invalid currency symbol: " ++ show cs ++ "; Reason: " ++ show x
          Right cs' -> return $ mintingPolicyIdFromApi cs'


instance Swagger.ToSchema GYMintingPolicyId where
  declareNamedSchema _ = pure $ Swagger.named "GYMintingPolicyId" $ mempty
                       & Swagger.type_           ?~ Swagger.SwaggerString
                       & Swagger.description     ?~ "This is the hash of a minting policy script."
                       & Swagger.format          ?~ "hex"
                       & Swagger.example         ?~ toJSON ("ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" :: Text)
                       & Swagger.maxLength       ?~ 56
                       & Swagger.minLength       ?~ 56

mintingPolicyIdToApi :: GYMintingPolicyId -> Api.PolicyId
mintingPolicyIdToApi = coerce

mintingPolicyIdFromApi :: Api.PolicyId -> GYMintingPolicyId
mintingPolicyIdFromApi = coerce

mintingPolicyIdCurrencySymbol :: GYMintingPolicyId -> Plutus.CurrencySymbol
mintingPolicyIdCurrencySymbol = coerce Conv.fromCardanoPolicyId

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

-- | Plutus script
data GYScript = GYScript
    !Plutus.Script
    (Api.PlutusScript Api.PlutusScriptV1)
    Plutus.ScriptHash
    Api.ScriptHash

-- | Equality and comparison are on script hash.
--
-- As hash is cryptographicly strong, and 'GYScript' constructor is not
-- exposed, this works great.
--
instance Eq GYScript where
    GYScript _ _ _ x == GYScript _ _ _ y = x == y

instance Ord GYScript where
    compare (GYScript _ _ _ x) (GYScript _ _ _ y) = compare x y

instance Show GYScript where
    showsPrec d (GYScript _ _ _ h) = showParen (d > 10)
        $ showString "GYScript "
        . showsPrec 11 h

-- In implementation we cache the api representation and hashes.

scriptFromPlutus :: Plutus.Script -> GYScript
scriptFromPlutus script =
    GYScript script apiPlutusScript plutusHash apiHash
  where
    -- implementation: Ledger.Scripts.toCardanoApiScript script
    apiPlutusScript
        = Api.S.PlutusScriptSerialised
        $ SBS.toShort
        $ BSL.toStrict
        $ Codec.Serialise.serialise script

    apiScript  = Api.PlutusScript Api.PlutusScriptV1 apiPlutusScript
    plutusHash = Ledger.Scripts.scriptHash script
    apiHash    = Api.hashScript apiScript

scriptToPlutus :: GYScript -> Plutus.Script
scriptToPlutus (GYScript p _ _ _) = p

scriptToApi :: GYScript -> Api.PlutusScript Api.PlutusScriptV1
scriptToApi (GYScript _ api _ _) = api

scriptFromApi :: Api.PlutusScript Api.PlutusScriptV1 -> GYScript
scriptFromApi = scriptFromPlutus . Codec.Serialise.deserialise . BSL.fromStrict . Api.serialiseToRawBytes

scriptPlutusHash :: GYScript -> Plutus.ScriptHash
scriptPlutusHash (GYScript _ _ ph _) = ph

scriptApiHash :: GYScript -> Api.ScriptHash
scriptApiHash (GYScript _ _ _ ah) = ah
