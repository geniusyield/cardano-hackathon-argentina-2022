module GeniusYield.Types.Redeemer (
    GYRedeemer (..),
    redeemerToApi,
    redeemerToPlutus,
) where

import qualified Cardano.Api          as Api
import qualified Plutus.V1.Ledger.Api as Plutus

data GYRedeemer where
    GYRedeemer :: (Plutus.ToData a, Show a) => a -> GYRedeemer

deriving instance Show GYRedeemer

redeemerToPlutus :: GYRedeemer -> Plutus.Redeemer
redeemerToPlutus (GYRedeemer x) = Plutus.Redeemer $ Plutus.toBuiltinData x

redeemerToApi :: GYRedeemer -> Api.ScriptData
redeemerToApi (GYRedeemer x) = dataToScriptData $ Plutus.toData x

dataToScriptData :: Plutus.Data -> Api.ScriptData
dataToScriptData (Plutus.Constr n xs) = Api.ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Plutus.Map xs)      = Api.ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (Plutus.List xs)     = Api.ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (Plutus.I n)         = Api.ScriptDataNumber n
dataToScriptData (Plutus.B bs)        = Api.ScriptDataBytes bs
