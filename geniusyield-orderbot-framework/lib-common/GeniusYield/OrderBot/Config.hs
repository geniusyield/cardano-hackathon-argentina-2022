{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.OrderBot.Config (
  GYCoreProviderInfo (..),
  GYCoreConfig (..),
  withCfgProviders,
) where

import Cardano.Api (CardanoMode, LocalNodeConnectInfo (..))
import Control.Exception (SomeException, try)
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.Aeson.Types as Aeson
import Data.Char
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PQ
import qualified Database.PostgreSQL.Simple.URL as PQ

import GeniusYield.Imports
import qualified GeniusYield.Providers.Blockfrost as Blockfrost
import qualified GeniusYield.Providers.CardanoDbSync as DbSync
import qualified GeniusYield.Providers.ChainIndex as ChainIndex
import qualified GeniusYield.Providers.Maestro as MaestroApi
import qualified GeniusYield.Providers.Node as Node
import qualified GeniusYield.Providers.SubmitApi as SubmitApi
import GeniusYield.Types

{- |
The supported providers. The options are:

- Local node.socket and a running chain index
- Cardano db sync alongside cardano submit api
- Blockfrost, provided an API key for the corresponding network id
- Maestro blockchain API, provided its URL and API token

In JSON format, this essentially corresponds to:

= { socketPath: FilePath, chainIndexUrl: string }
| { cardanoDbSync: PQ.ConnectInfo, cardanoSubmitApiUrl: string }
| { blockfrostKey: string }
| { maestroUrl: string, maestroToken: int }

The constructor tags don't need to appear in the JSON.
-}
data GYCoreProviderInfo
  = GYNodeChainIx {cpiSocketPath :: !FilePath, cpiChainIndexUrl :: !String}
  | GYDbSync {cpiCardanoDbSync :: !PQConnInf, cpiCardanoSubmitApiUrl :: !String}
  | GYBlockfrost {cpiBlockfrostKey :: !String}
  | GYMaestro {cpiMaestroUrl :: !String, cpiMaestroToken :: !Int}
  deriving stock (Show)

{- |
The config to initialize the GY framework with.
Should include information on the providers to use, as well as the network id.

In JSON format, this essentially corresponds to:

= { coreProvider: GYCoreProviderInfo, networkId: NetworkId }
-}
data GYCoreConfig = GYCoreConfig
  { cfgCoreProvider :: !GYCoreProviderInfo
  , cfgNetworkId :: !GYNetworkId
  }
  deriving stock (Show)

nodeConnectInfo :: FilePath -> GYNetworkId -> LocalNodeConnectInfo CardanoMode
nodeConnectInfo path netId = networkIdToLocalNodeConnectInfo netId path

withCfgProviders :: GYCoreConfig -> (GYProviders -> IO a) -> IO a
withCfgProviders GYCoreConfig {cfgCoreProvider, cfgNetworkId} f = do
  (gyGetParameters, gySlotActions, gyQueryUTxO, gyLookupDatum, gySubmitTx) <- case cfgCoreProvider of
    GYNodeChainIx path chainIxUrl -> do
      let info = nodeConnectInfo path cfgNetworkId
          era = networkIdToEra cfgNetworkId
      chainIxEnv <- ChainIndex.newChainIndexEnv chainIxUrl
      pure
        ( Node.nodeGetParameters era info
        , Node.nodeSlotActions info
        , Node.nodeQueryUTxO era info
        , ChainIndex.chainIndexLookupDatum chainIxEnv
        , Node.nodeSubmitTx info
        )
    GYDbSync (PQConnInf ci) submitUrl -> do
      -- NOTE: This provider generally does not support anything other than private testnets.
      conn <- DbSync.openDbSyncConn ci
      submitApiEnv <- SubmitApi.newSubmitApiEnv submitUrl
      pure
        ( DbSync.dbSyncGetParameters conn
        , DbSync.dbSyncSlotActions conn
        , DbSync.dbSyncQueryUtxo conn
        , DbSync.dbSyncLookupDatum conn
        , SubmitApi.submitApiSubmitTxDefault submitApiEnv
        )
    GYBlockfrost key -> do
      -- NOTE: This provider only has proper support for the preprod testnet as of now.
      let proj = networkIdToProject cfgNetworkId key
      -- Obtain these _unlikely to change_ constants once so that subsequenct calls will simply return them.
      pparams <- Blockfrost.blockfrostProtocolParams proj
      eraHist <- Blockfrost.blockfrostEraHistory proj
      sysStart <- Blockfrost.blockfrostSystemStart proj
      -- Build the GYGetParameters with the constants:
      let blockfrostGetParams =
            GYGetParameters
              { gyGetProtocolParameters' = pure pparams
              , gyGetSystemStart' = pure sysStart
              , gyGetEraHistory' = pure eraHist
              , gyGetStakePools' = Blockfrost.blockfrostStakePools proj
              }
      pure
        ( blockfrostGetParams
        , Blockfrost.blockfrostSlotActions proj
        , Blockfrost.blockfrostQueryUtxo proj
        , Blockfrost.blockfrostLookupDatum proj
        , Blockfrost.blockfrostSubmitTx proj
        )
    GYMaestro maestroUrl apiToken -> do
      -- NOTE: This provider only supports the preprod testnet. Other networks will immediately fail.
      unless (cfgNetworkId == GYTestnetPreprod)
        . throwIO
        $ userError "Maestro provider currently only supports the preprod testnet."
      maestroApiEnv <- MaestroApi.newMaestroApiEnv maestroUrl apiToken
      -- Obtain these _unlikely to change_ constants once so that subsequenct calls will simply return them.
      pparams <- MaestroApi.maestroProtocolParams
      eraHist <- MaestroApi.maestroEraHistory
      sysStart <- MaestroApi.maestroSystemStart maestroApiEnv
      -- Build the GYGetParameters with the constants:
      let maestroGetParams =
            GYGetParameters
              { gyGetProtocolParameters' = pure pparams
              , gyGetSystemStart' = pure sysStart
              , gyGetEraHistory' = pure eraHist
              , gyGetStakePools' = MaestroApi.maestroStakePools maestroApiEnv
              }
      pure
        ( maestroGetParams
        , MaestroApi.maestroSlotActions maestroApiEnv
        , MaestroApi.maestroQueryUtxo maestroApiEnv
        , MaestroApi.maestroLookupDatum maestroApiEnv
        , MaestroApi.maestroSubmitTx maestroApiEnv
        )

  let gyLog' = simpleConsoleLogging putStrLn
  e <- try $ f GYProviders {..}
  case e of
    Right a -> return a
    Left (err :: SomeException) -> do
      logRun gyLog' mempty GYError $ printf "ERROR: %s" $ show err
      throwIO err

newtype PQConnInf = PQConnInf PQ.ConnectInfo
  deriving newtype (Show)

instance FromJSON PQConnInf where
  parseJSON = Aeson.withText "ConnectInfo URL" $ \t ->
    case PQConnInf <$> PQ.parseDatabaseUrl (T.unpack t) of
      Nothing -> fail "Invalid PostgreSQL URL"
      Just ci -> pure ci

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      , sumEncoding = UntaggedValue
      }
    ''GYCoreProviderInfo
 )

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      }
    ''GYCoreConfig
 )
