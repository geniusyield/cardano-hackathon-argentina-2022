module GeniusYield.Providers.ChainIndex
    ( ChainIndexEnv
    , newChainIndexEnv
    , chainIndexLookupDatum
    ) where

import Control.Exception           (throwIO)
import Data.Proxy                  (Proxy (..))
import Network.HTTP.Client         (defaultManagerSettings, newManager)
import Plutus.ChainIndex.Api       (API)
import Plutus.V1.Ledger.Scripts    qualified as Plutus
import Servant.API                 ((:<|>) (..))
import Servant.Client              (ClientEnv, ClientError (..), ClientM, client, mkClientEnv, parseBaseUrl, runClientM)

import GeniusYield.Types

-- | chain-index environment
newtype ChainIndexEnv = ChainIndexEnv ClientEnv

newChainIndexEnv :: String -> IO ChainIndexEnv
newChainIndexEnv baseUrl = do
    manager <- newManager defaultManagerSettings
    url     <- parseBaseUrl baseUrl
    return $ ChainIndexEnv $ mkClientEnv manager url

datum :: Plutus.DatumHash -> ClientM Plutus.Datum
_ :<|> (datum :<|> _) :<|> _ = client @API Proxy

chainIndexLookupDatum :: ChainIndexEnv -> GYLookupDatum
chainIndexLookupDatum (ChainIndexEnv env) dh = do
    e <- runClientM (datum $ datumHashToPlutus dh) env
    case e of
        Left (FailureResponse _ _) -> return Nothing            -- no Datum belonging to the DatumHash could be found
        Left err                   -> throwIO err               -- something is wrong with our connection to the chain indexer
        Right (Plutus.Datum d)     -> return $ Just $ GYDatum d -- we found the Datum
