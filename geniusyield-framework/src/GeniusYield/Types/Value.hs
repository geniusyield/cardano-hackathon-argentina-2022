{-# LANGUAGE OverloadedLists #-}
module GeniusYield.Types.Value (
    -- * Value
    GYValue,
    valueToPlutus,
    valueFromPlutus,
    valueToApi,
    valueFromApi,
    valueSingleton,
    valueFromList,
    valueToList,
    valueMap,
    valueFromLovelace,
    valueFromApiTxOutValue,
    valueToApiTxOutValue,
    -- ** Arithmetic
    valueMinus,
    valueNegate,
    valueNonNegative,
    valueGreaterOrEqual,
    valueSplitAda,
    valueAssetClass,
    -- ** Predicates
    isEmptyValue,
    -- ** Debug
    valueValid,
    -- * Asset class
    GYAssetClass (..),
    assetClassToPlutus,
    assetClassFromPlutus,
    -- * Token name
    GYTokenName(..),
    tokenNameFromBS,
    tokenNameToPlutus,
    tokenNameFromPlutus,
    tokenNameFromHex,
    tokenNameFromHexUnsafe,
    makeAssetClass
) where

import           Data.List                        (intercalate)
import           Control.Lens                     ((&), (?~), (.~))
import           GeniusYield.Imports
import           PlutusTx.Builtins.Class          (fromBuiltin, toBuiltin)

import qualified Cardano.Api                      as Api
import qualified Data.Aeson.Types                 as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Base16           as Base16
import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import qualified Plutus.V1.Ledger.Value           as Plutus
import qualified Text.Printf                      as Printf
import qualified Web.HttpApiData                  as Web
import qualified Data.Swagger                     as Swagger
import qualified Data.Swagger.Internal.Schema     as Swagger

import qualified GeniusYield.Types.Ada            as Ada
import           GeniusYield.Types.Script

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Cardano.Api                as Api
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import qualified Text.Printf                as Printf
-- >>> import qualified Web.HttpApiData            as Web

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

-- | Value: a (total) map from asset classes ('GYAssetClass') to amount ('Integer').
newtype GYValue = GYValue (Map.Map GYAssetClass Integer)
  deriving (Eq)
  deriving newtype (Swagger.ToSchema)

-- | Check the 'GYValue' representation invariants.
--
-- Should always evaluate to 'True'
valueValid :: GYValue -> Bool
valueValid (GYValue v) = all (/= 0) v -- invariant: zero integers are not stored.

-- This normalizes the map.
valueMake :: Map.Map GYAssetClass Integer -> GYValue
valueMake m = GYValue (Map.filter (/= 0) m)

instance Show GYValue where
    showsPrec d v = showParen (d > 10)
        $ showString "valueFromList "
        . showsPrec 11 (valueToList v)

instance Semigroup GYValue where
    GYValue x <> GYValue y = valueMake $ Map.unionWith (+) x y

instance Monoid GYValue where
    mempty = GYValue Map.empty

valueToPlutus :: GYValue -> Plutus.Value
valueToPlutus (GYValue m) = foldMap f (Map.toList m) where
    f :: (GYAssetClass, Integer) -> Plutus.Value
    f (assetClassToPlutus -> Plutus.AssetClass (cs, tn), n) = Plutus.singleton cs tn n

valueFromPlutus ::  Plutus.Value -> Maybe GYValue
valueFromPlutus v = fmap valueFromList $
    forM (Plutus.flattenValue v) $ \(cs, tn, n) -> do
        ac <- assetClassFromPlutus (Plutus.AssetClass (cs, tn))
        return (ac, n)

-- |
--
-- >>> valueFromLovelace 0
-- valueFromList []
--
-- >>> valueFromLovelace 100
-- valueFromList [(GYLovelace,100)]
--
valueFromLovelace :: Integer -> GYValue
valueFromLovelace 0 = GYValue mempty
valueFromLovelace i = GYValue (Map.singleton GYLovelace i)

valueSingleton :: GYAssetClass -> Integer -> GYValue
valueSingleton ac n = valueMake $ Map.singleton ac n

valueToApi :: GYValue -> Api.Value
valueToApi v = Api.valueFromList
    [ (assetClassToApi ac, Api.Quantity n)
    | (ac, n) <- valueToList v
    ]

valueFromApi :: Api.Value -> GYValue
valueFromApi v = valueFromList
    [ (assetClassFromApi ac, n)
    | (ac, Api.Quantity n) <- Api.valueToList v
    ]

valueFromApiTxOutValue :: Api.TxOutValue era -> GYValue
valueFromApiTxOutValue (Api.TxOutValue _ v)                  = valueFromApi v
valueFromApiTxOutValue (Api.TxOutAdaOnly _ (Api.Lovelace x)) = valueFromLovelace x

valueToApiTxOutValue :: GYValue -> Api.TxOutValue Api.AlonzoEra
valueToApiTxOutValue v = Api.TxOutValue Api.MultiAssetInAlonzoEra (valueToApi v)

-- | Create 'GYValue' from a list of asset class and count.
--
-- Duplicates are merged.
--
valueFromList :: [(GYAssetClass, Integer)] -> GYValue
valueFromList xs = valueMake $ Map.fromListWith (+) xs

valueToList :: GYValue -> [(GYAssetClass, Integer)]
valueToList (GYValue v) = Map.toList v

valueMap :: (GYAssetClass -> Integer -> Integer) -> GYValue -> GYValue
valueMap f (GYValue m) = GYValue (Map.mapWithKey f m)

-- |
--
-- >>> Printf.printf "value = %s" (valueFromList [])
-- value =
--
-- >>> Printf.printf "value = %s" (valueFromList [(GYLovelace, 1000)])
-- value = 1000 lovelace
--
instance Printf.PrintfArg GYValue where
    formatArg v ff = Printf.formatArg (showValue (valueToPlutus v)) ff

showValue :: Plutus.Value -> String
showValue = intercalate " + " . map f . Plutus.flattenValue
  where
    f :: (Plutus.CurrencySymbol, Plutus.TokenName, Integer) -> String
    f (cs, tn, n) = show n ++ " " ++ showAssetClass (Plutus.AssetClass (cs, tn))

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode $ valueFromList [(GYLovelace, 1000)]
-- "1000 lovelace"
--
-- /Note:/ this is a weird instance, is hard to parse, but it is used in @ToJSON OrderInfoExtended@.
--
instance Aeson.ToJSON GYValue where
    toJSON = Aeson.toJSON . showValue . valueToPlutus

-------------------------------------------------------------------------------
-- Arithmetic
-------------------------------------------------------------------------------

valueMinus :: GYValue -> GYValue -> GYValue
valueMinus x y = x <> valueNegate y

valueNegate :: GYValue -> GYValue
valueNegate (GYValue x) = GYValue (Map.map negate x)

valueNonNegative :: GYValue -> Bool
valueNonNegative (GYValue x) = all (>= 0) x

valueGreaterOrEqual :: GYValue -> GYValue -> Bool
valueGreaterOrEqual v w = valueNonNegative $ v `valueMinus` w

-- |
--
-- >>> valueSplitAda $ valueFromLovelace 100
-- (100,valueFromList [])
--
valueSplitAda :: GYValue -> (Integer, GYValue)
valueSplitAda (GYValue m) = (Map.findWithDefault 0 GYLovelace m, GYValue (Map.delete GYLovelace m))

valueAssetClass :: GYValue -> GYAssetClass -> Integer
valueAssetClass (GYValue m) ac = Map.findWithDefault 0 ac m

-------------------------------------------------------------------------------
-- Predicates
-------------------------------------------------------------------------------

-- |
--
-- >>> isEmptyValue mempty
-- True
--
-- >>> isEmptyValue $ valueFromLovelace 100
-- False
--
-- >>> isEmptyValue $ valueMinus (valueFromLovelace 100) (valueFromLovelace 100)
-- True
--
isEmptyValue :: GYValue -> Bool
isEmptyValue (GYValue m) = Map.null m

-------------------------------------------------------------------------------
-- Asset class
-------------------------------------------------------------------------------

-- | Asset class. Either lovelace or minted token.
--
data GYAssetClass = GYLovelace | GYToken GYMintingPolicyId GYTokenName
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSONKey GYAssetClass

instance Swagger.ToSchema GYAssetClass where
  declareNamedSchema _ = do
                           unitSchema   <- Swagger.declareSchemaRef @()     Proxy
                           stringSchema <- Swagger.declareSchemaRef @String Proxy
                           return $ Swagger.named "GYAssetClass" $ mempty
                             & Swagger.type_         ?~ Swagger.SwaggerString
                             & Swagger.description   ?~ "This is an asset class, i.e. either lovelace or some other token with its minting policy and token name."
                             & Swagger.properties .~ [ ("GYLovelace" :: Text, unitSchema)
                                                     , ("GYToken" :: Text, stringSchema)
                                                     ]
                             & Swagger.minProperties ?~ 1
                             & Swagger.maxProperties ?~ 1
                             & Swagger.example       ?~ toJSON ("ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.GOLD" :: Text)


assetClassToPlutus :: GYAssetClass -> Plutus.AssetClass
assetClassToPlutus GYLovelace      = Plutus.AssetClass (Ada.adaSymbol, Ada.adaToken)
assetClassToPlutus (GYToken cs tn) = Plutus.AssetClass (mintingPolicyIdCurrencySymbol cs, tokenNameToPlutus tn)

assetClassFromPlutus :: Plutus.AssetClass -> Maybe GYAssetClass
assetClassFromPlutus (Plutus.AssetClass (cs, tn))
    | cs == Ada.adaSymbol, tn == Ada.adaToken  = Just GYLovelace
    | otherwise                                = do
        tn' <- tokenNameFromPlutus tn
        cs' <- Api.deserialiseFromRawBytes Api.AsScriptHash $
            case cs of Plutus.CurrencySymbol bs -> fromBuiltin bs
        return (GYToken (mintingPolicyIdFromApi (Api.PolicyId cs')) tn')

assetClassToApi :: GYAssetClass -> Api.AssetId
assetClassToApi GYLovelace          = Api.AdaAssetId
assetClassToApi (GYToken cs tn) = Api.AssetId (mintingPolicyIdToApi cs) (tokenNameToApi tn)

assetClassFromApi :: Api.AssetId -> GYAssetClass
assetClassFromApi Api.AdaAssetId      = GYLovelace
assetClassFromApi (Api.AssetId cs tn) = GYToken (mintingPolicyIdFromApi cs) (tokenNameFromApi tn)

instance IsString GYAssetClass where
    fromString s = case Web.parseUrlPiece $ T.pack s of
        Left err -> error $ T.unpack err
        Right x  -> x

-- |
--
-- >>> Printf.printf "ac = %s" GYLovelace
-- ac = lovelace
--
instance Printf.PrintfArg GYAssetClass where
    formatArg ac ff = Printf.formatArg (showAssetClass (assetClassToPlutus ac)) ff

showAssetClass :: Plutus.AssetClass -> String
showAssetClass (Plutus.AssetClass (cs, tn))
    | cs == Ada.adaSymbol && tn == Ada.adaToken = "lovelace"
    | otherwise
     = show cs ++ "." ++ init (tail $ show tn)

-- | Note: not used currently by API (tests only)
--
-- >>> Web.toUrlPiece GYLovelace
-- "lovelace"
--
instance Web.ToHttpApiData GYAssetClass where
    toUrlPiece = T.pack . showAssetClass . assetClassToPlutus

-- | Note: not used currently by API (tests only)
--
-- >>> Web.parseUrlPiece @GYAssetClass "lovelace"
-- Right GYLovelace
--
-- >>> Web.parseUrlPiece @GYAssetClass "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.GOLD"
-- Right (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD")
--
instance Web.FromHttpApiData GYAssetClass where
    parseUrlPiece t = first T.pack (parseAssetClass t)

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode GYLovelace
-- "lovelace"
--
instance Aeson.ToJSON GYAssetClass where
    toJSON = Aeson.toJSON . showAssetClass . assetClassToPlutus

-- |
--
-- >>> Aeson.decode @GYAssetClass "\"lovelace\""
-- Just GYLovelace
--
-- >>> Aeson.decode @GYAssetClass "\"ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.GOLD\""
-- Just (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD")
--
instance Aeson.FromJSON GYAssetClass where
    parseJSON (Aeson.String t) = either fail return (parseAssetClass t)
    parseJSON v                = Aeson.typeMismatch "AssetClass" v

parseAssetClass :: Text -> Either String GYAssetClass
parseAssetClass "lovelace" = return GYLovelace
parseAssetClass t          = Atto.parseOnly parser (TE.encodeUtf8 t)
  where
    parser :: Atto.Parser GYAssetClass
    parser = do
        cs <- Atto.takeWhile1 isHexDigit
        _  <- Atto.char '.'
        tn <- Atto.takeWhile1 isAlphaNum
        case Api.deserialiseFromRawBytesHex Api.AsPolicyId cs of
            Left x                   -> fail $ "Invalid currency symbol: " ++ show cs ++ "; Reason: " ++ show x
            Right cs'
                | BS.length tn >= 32 -> fail $ "Token name too long " ++ show tn
                | otherwise          -> return $ GYToken (mintingPolicyIdFromApi cs') (GYTokenName tn)

-------------------------------------------------------------------------------
-- TokenName
-------------------------------------------------------------------------------

-- | Token name is an arbitrary byte string up to 32 bytes long.
--
-- TODO: it's unclear whether it's an arbitrary byte string
-- or UTF8 encoded text (which encoded byte form is 32 byte long at most).
-- /We treat it as an arbitrary string/.
--
-- >>> LBS8.putStrLn $ Aeson.encode ("Gold" :: GYTokenName)
-- "476f6c64"
--
newtype GYTokenName = GYTokenName BS.ByteString
    deriving stock (Eq, Ord)

instance Show GYTokenName where
    showsPrec d (GYTokenName s) = showsPrec d s

-- | /Does NOT UTF8-encode/.
instance IsString GYTokenName where
    fromString s = fromMaybe
        (error $ "fromString @GYTokenName " ++ show s ++ ": token name too long")
        (tokenNameFromBS bs)
      where
        bs = fromString s -- TODO: utf8-encode


instance Swagger.ToParamSchema GYTokenName where
    toParamSchema _ = mempty
        & Swagger.type_     ?~ Swagger.SwaggerString
        & Swagger.maxLength ?~ 64
        & Swagger.format    ?~ "hex"
        & Swagger.pattern   ?~ "[0-9a-fA-F]+"

instance Swagger.ToSchema GYTokenName where
    declareNamedSchema _ = pure $ Swagger.named "GYTokenName" $ Swagger.paramSchemaToSchema (Proxy @GYTokenName)
        & Swagger.description ?~ "This is the name of a token."
        & Swagger.example     ?~ toJSON ("476f6c64" :: Text)

-- |
--
-- >>> Aeson.eitherDecode @GYTokenName "\"476f6c64\""
-- Right "Gold"
--
-- >>> Aeson.eitherDecode @GYTokenName "\"0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f2021\""
-- Left "Error in $: parseJSON @GYTokenName: token name too long (0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f2021)"
--
-- >>> Aeson.eitherDecode @GYTokenName "\"gold\""
-- Left "Error in $: parseJSON @GYTokenName: not base16 encoded (gold)"
--
-- >>> Aeson.eitherDecode @GYTokenName "123"
-- Left "Error in $: parsing Text failed, expected String, but encountered Number"
--
instance Aeson.FromJSON GYTokenName where
    parseJSON v = do
        t <- parseJSON v
        case Web.parseUrlPiece t of
            Right tn -> return tn
            Left err -> fail $ "parseJSON @GYTokenName: " <> T.unpack err

-- |
--
-- >>> Aeson.encode @GYTokenName "Gold"
-- "\"476f6c64\""
--
instance Aeson.ToJSON GYTokenName where
    toJSON (GYTokenName bs) = Aeson.String (TE.decodeUtf8 (Base16.encode bs))

-- |
--
-- >>> Web.parseUrlPiece @GYTokenName "476f6c64"
-- Right "Gold"
--
-- >>> Web.parseUrlPiece @GYTokenName "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f2021"
-- Left "token name too long (0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f2021)"
--
-- >>> Web.parseUrlPiece @GYTokenName "Gold"
-- Left "not base16 encoded (Gold)"
--
instance Web.FromHttpApiData GYTokenName where
    parseUrlPiece t = case Base16.decode $ TE.encodeUtf8 t of
        Right bs -> maybe (Left $ "token name too long (" <> t <> ")") Right $ tokenNameFromBS bs
        Left _   -> Left $ "not base16 encoded (" <> t <> ")"

tokenNameToPlutus :: GYTokenName -> Plutus.TokenName
tokenNameToPlutus (GYTokenName bs) = Plutus.TokenName (toBuiltin bs)

-- | Convert Plutus 'Plutus.TokenName' to 'GYTokenName'.
tokenNameFromPlutus :: HasCallStack => Plutus.TokenName -> Maybe GYTokenName
tokenNameFromPlutus (Plutus.TokenName bbs) = tokenNameFromBS (fromBuiltin bbs)

tokenNameFromBS :: BS.ByteString -> Maybe GYTokenName
tokenNameFromBS bs
    | BS.length bs > 32 = Nothing
    | otherwise         = Just (GYTokenName bs)

tokenNameToApi :: GYTokenName -> Api.AssetName
tokenNameToApi = coerce

tokenNameFromApi :: Api.AssetName -> GYTokenName
tokenNameFromApi = coerce

tokenNameFromHex :: Text -> Either Text GYTokenName
tokenNameFromHex = Web.parseUrlPiece

tokenNameFromHexUnsafe :: Text -> GYTokenName
tokenNameFromHexUnsafe = either (error . T.unpack) id . tokenNameFromHex

makeAssetClass :: Text -> Text -> Maybe GYAssetClass
makeAssetClass sym tn = assetClassFromPlutus $ Plutus.AssetClass (fromString $ T.unpack $ norm sym, fromString $ T.unpack $ norm tn)
  where
    norm :: Text -> Text
    norm s
        | s == "ADA" = ""
        | otherwise  = s
