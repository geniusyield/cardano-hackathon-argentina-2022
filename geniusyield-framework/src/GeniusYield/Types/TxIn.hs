module GeniusYield.Types.TxIn (
    GYTxIn (..),
    txInToApi,
) where

import qualified Cardano.Api                as Api
import qualified Cardano.Api.Shelley        as Api.S

import           GeniusYield.Types.Datum
import           GeniusYield.Types.Redeemer
import           GeniusYield.Types.Script
import           GeniusYield.Types.TxOutRef

-- | Transaction input:
--
-- * an UTxO
--
-- * non-key witness for script utxos
--
data GYTxIn = GYTxIn
    { gyTxInTxOutRef :: !GYTxOutRef
    , gyTxInWitness  :: !(Maybe (GYValidator, GYDatum, GYRedeemer))
    }
  deriving Show

-- |
--
-- /Note:/ @TxIns@ type synonym is not exported: https://github.com/input-output-hk/cardano-node/issues/3732
txInToApi :: GYTxIn -> (Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxTxIn Api.AlonzoEra))
txInToApi (GYTxIn oref m) = (txOutRefToApi oref, Api.BuildTxWith $ f m) where
    f :: Maybe (GYValidator, GYDatum, GYRedeemer) -> Api.Witness Api.WitCtxTxIn Api.AlonzoEra
    f Nothing          = Api.KeyWitness Api.KeyWitnessForSpending
    f (Just (x, d, r)) = Api.ScriptWitness Api.ScriptWitnessForSpending $ Api.PlutusScriptWitness
        Api.PlutusScriptV1InAlonzo
        Api.PlutusScriptV1
        -- FIXME(VASIL): Reference script support.
        (Api.S.PScript $ validatorToApi x)
        (Api.ScriptDatumForTxIn $ datumToApi' d)
        (redeemerToApi r)
        (Api.ExecutionUnits 0 0)
