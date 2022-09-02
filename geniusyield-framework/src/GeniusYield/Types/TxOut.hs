module GeniusYield.Types.TxOut (
    GYTxOut (..),
    txOutToApi,
) where

import qualified Cardano.Api               as Api
import qualified Cardano.Api.Shelley       as Api.S

import           GeniusYield.Types.Address
import           GeniusYield.Types.Datum
import           GeniusYield.Types.Value

data GYTxOut = GYTxOut
    { gyTxOutAddress :: !GYAddress
    , gyTxOutValue   :: !GYValue
    , gyTxOutDatum   :: !(Maybe GYDatum)
    } deriving stock (Show)


txOutToApi :: GYTxOut -> Api.TxOut Api.CtxTx Api.AlonzoEra
txOutToApi (GYTxOut addr v md) = Api.TxOut
    (addressToApi' addr)
    (Api.TxOutValue Api.MultiAssetInAlonzoEra $ valueToApi v)
    -- FIXME(VASIL): Inline datum support.
    (maybe Api.TxOutDatumNone (Api.TxOutDatumInTx Api.ScriptDataInAlonzoEra . datumToApi') md)
    -- FIXME(VASIL): Reference script support.
    Api.S.ReferenceScriptNone
