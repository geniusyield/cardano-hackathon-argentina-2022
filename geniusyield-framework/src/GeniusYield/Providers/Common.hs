module GeniusYield.Providers.Common (SomeDeserializeError (..), newServantClientEnv) where

import qualified Cardano.Api as Api
import qualified Servant.Client as Servant
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpClientTLS

data SomeDeserializeError
    = DeserializeErrorBech32 Api.Bech32DecodeError
    | DeserializeErrorAeson String
    | DeserializeErrorScriptDataJson Api.ScriptDataJsonError
    deriving stock (Eq, Show)

newServantClientEnv :: String -> IO Servant.ClientEnv
newServantClientEnv baseUrl = do
    url     <- Servant.parseBaseUrl baseUrl
    manager <- if Servant.baseUrlScheme url == Servant.Https
        then HttpClient.newManager HttpClientTLS.tlsManagerSettings
        else HttpClient.newManager HttpClient.defaultManagerSettings
    pure $ Servant.mkClientEnv manager url
