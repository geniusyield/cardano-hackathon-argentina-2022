module GeniusYield.Types.Datum (
    -- * Datum
    GYDatum (..),
    datumToApi',
    datumFromApi',
    datumToPlutus,
    datumToPlutus',
    datumFromPlutus,
    datumFromPlutus',
    hashDatum,
    -- * Datum hash
    GYDatumHash,
    datumHashFromPlutus,
    unsafeDatumHashFromPlutus,
    datumHashToPlutus,
    datumHashFromApi,
    datumHashToApi,
) where

import qualified Cardano.Api                          as Api
import qualified Database.PostgreSQL.Simple           as PQ
import qualified Database.PostgreSQL.Simple.FromField as PQ (FromField (..), returnError)
import qualified Database.PostgreSQL.Simple.ToField   as PQ
import qualified Ledger.Tx.CardanoAPI                 as Conv
import qualified Plutus.V1.Ledger.Api                 as Plutus

import           GeniusYield.Imports

-- | Datum
--
-- In the GY system we always include datums in transactions
-- so this simple type is sufficient.
--
data GYDatum where
    GYDatum :: (Plutus.ToData a, Show a) => a -> GYDatum

deriving instance Show GYDatum

datumToApi' :: GYDatum -> Api.ScriptData
datumToApi' (GYDatum x) = dataToScriptData $ Plutus.toData x

datumFromApi' :: Api.ScriptData -> GYDatum
datumFromApi' x = GYDatum (Conv.fromCardanoScriptData x)

datumToPlutus :: GYDatum -> Plutus.Datum
datumToPlutus = Plutus.Datum . datumToPlutus'

datumToPlutus' :: GYDatum -> Plutus.BuiltinData
datumToPlutus' (GYDatum x) = Plutus.toBuiltinData x

datumFromPlutus :: Plutus.Datum -> GYDatum
datumFromPlutus (Plutus.Datum d) = GYDatum d

datumFromPlutus' :: Plutus.BuiltinData -> GYDatum
datumFromPlutus' = GYDatum

dataToScriptData :: Plutus.Data -> Api.ScriptData
dataToScriptData (Plutus.Constr n xs) = Api.ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Plutus.Map xs)      = Api.ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (Plutus.List xs)     = Api.ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (Plutus.I n)         = Api.ScriptDataNumber n
dataToScriptData (Plutus.B bs)        = Api.ScriptDataBytes bs

hashDatum :: GYDatum -> GYDatumHash
hashDatum = datumHashFromApi . Api.hashScriptData . datumToApi'

-------------------------------------------------------------------------------
-- DatumHash
-------------------------------------------------------------------------------

newtype GYDatumHash = GYDatumHash (Api.Hash Api.ScriptData)
    deriving stock   (Show)
    deriving newtype (Eq, Ord, ToJSON)

instance IsString GYDatumHash where
    fromString = unsafeDatumHashFromPlutus . fromString

instance PQ.FromField GYDatumHash where
    fromField f bs' = do
        PQ.Binary bs <- PQ.fromField f bs'
        case Api.deserialiseFromRawBytes (Api.AsHash Api.AsScriptData) bs of
            Just dh -> return (datumHashFromApi dh)
            Nothing -> PQ.returnError PQ.ConversionFailed f "datum hash does not unserialise"

instance PQ.ToField GYDatumHash where
    toField (GYDatumHash dh) = PQ.toField (PQ.Binary (Api.serialiseToRawBytes dh))

datumHashFromPlutus :: Plutus.DatumHash -> Maybe GYDatumHash
datumHashFromPlutus h = case Conv.toCardanoScriptDataHash h of
    Left _err -> Nothing
    Right x   -> Just (datumHashFromApi x)

unsafeDatumHashFromPlutus :: Plutus.DatumHash -> GYDatumHash
unsafeDatumHashFromPlutus h = case Conv.toCardanoScriptDataHash h of
    Left err -> error $ "unsafeDatumHashFromPlutus: " ++ show err
    Right x  -> datumHashFromApi x

-- TODO: remove me
datumHashToPlutus :: GYDatumHash -> Plutus.DatumHash
datumHashToPlutus h = Plutus.DatumHash (Plutus.toBuiltin (Api.serialiseToRawBytes (datumHashToApi h)))

datumHashFromApi :: Api.Hash Api.ScriptData -> GYDatumHash
datumHashFromApi = coerce

datumHashToApi :: GYDatumHash -> Api.Hash Api.ScriptData
datumHashToApi = coerce
