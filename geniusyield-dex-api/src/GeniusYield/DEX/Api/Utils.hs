module GeniusYield.DEX.Api.Utils (
  maybeToExceptT,
  expectedTokenName,
) where

import Control.Monad.Except (ExceptT (..))
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusTx.Builtins as Plutus

import GeniusYield.Types

maybeToExceptT :: Functor m => e -> m (Maybe a) -> ExceptT e m a
maybeToExceptT err m = ExceptT $ fmap (maybe (Left err) Right) m

expectedTokenName :: GYTxOutRef -> Maybe GYTokenName
expectedTokenName ref = tokenNameFromPlutus $ Plutus.TokenName s
  where
    Plutus.TxOutRef (Plutus.TxId tid) ix = txOutRefToPlutus ref

    s :: Plutus.BuiltinByteString
    s = Plutus.sha2_256 (Plutus.consByteString ix tid)
