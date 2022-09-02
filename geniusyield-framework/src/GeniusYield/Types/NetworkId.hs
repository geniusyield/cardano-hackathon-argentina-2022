module GeniusYield.Types.NetworkId
    ( GYNetworkId (..)
    , fromApiNetworkId
    , toApiNetworkId
    , networkIdToLocalNodeConnectInfo
    , networkIdToProject
    , networkIdToEra
    ) where

import qualified Blockfrost.Client as BlockfrostClient
import qualified Cardano.Api       as Api
import qualified Data.Aeson.Types  as Aeson
import qualified Data.Text         as T

import GeniusYield.Types.Era

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8

data GYNetworkId
    = GYMainnet        -- ^ cardano mainnet
    | GYTestnetPreprod -- ^ cardano preprod testnet
    | GYTestnetPreview -- ^ cardano preview testnet
    | GYTestnetLegacy  -- ^ cardano legacy testnet
    | GYPrivnet        -- ^ local private network
    deriving (Show, Read, Eq, Ord)

fromApiNetworkId :: Api.NetworkId -> Maybe GYNetworkId
fromApiNetworkId Api.Mainnet                                 = Just GYMainnet
fromApiNetworkId (Api.Testnet (Api.NetworkMagic 1))          = Just GYTestnetPreprod
fromApiNetworkId (Api.Testnet (Api.NetworkMagic 2))          = Just GYTestnetPreview
fromApiNetworkId (Api.Testnet (Api.NetworkMagic 1097911063)) = Just GYTestnetLegacy
fromApiNetworkId (Api.Testnet (Api.NetworkMagic 42))         = Just GYPrivnet
fromApiNetworkId (Api.Testnet (Api.NetworkMagic _))          = Nothing

toApiNetworkId :: GYNetworkId -> Api.NetworkId
toApiNetworkId GYMainnet        = Api.Mainnet
toApiNetworkId GYTestnetPreprod = Api.Testnet $ Api.NetworkMagic 1
toApiNetworkId GYTestnetPreview = Api.Testnet $ Api.NetworkMagic 2
toApiNetworkId GYTestnetLegacy  = Api.Testnet $ Api.NetworkMagic 1097911063
toApiNetworkId GYPrivnet        = Api.Testnet $ Api.NetworkMagic 42

networkIdToEpochSlots :: GYNetworkId -> Api.EpochSlots
networkIdToEpochSlots GYPrivnet        = Api.EpochSlots 500
networkIdToEpochSlots GYMainnet        = Api.EpochSlots 432000
networkIdToEpochSlots GYTestnetPreprod = Api.EpochSlots 432000
networkIdToEpochSlots GYTestnetPreview = Api.EpochSlots 86400
networkIdToEpochSlots GYTestnetLegacy  = Api.EpochSlots 432000

-- This needs to be updated whenever a hardfork happens.
networkIdToEra :: GYNetworkId -> GYEra
networkIdToEra GYPrivnet        = GYBabbage
networkIdToEra GYMainnet        = GYAlonzo
networkIdToEra GYTestnetPreprod = GYAlonzo
networkIdToEra GYTestnetPreview = GYBabbage
networkIdToEra GYTestnetLegacy  = GYBabbage

networkIdGYToBlockfrost :: GYNetworkId -> BlockfrostClient.Env
networkIdGYToBlockfrost GYMainnet        = BlockfrostClient.Mainnet
networkIdGYToBlockfrost GYTestnetPreprod = BlockfrostClient.Preprod
networkIdGYToBlockfrost GYTestnetPreview = BlockfrostClient.Preview
networkIdGYToBlockfrost GYTestnetLegacy  = BlockfrostClient.Testnet
-- TODO: we need another mechanism to query private network data
networkIdGYToBlockfrost GYPrivnet        = error "Private network is not supported by Blockfrost"

-- | Constructs the connection info to a local node.
--
networkIdToLocalNodeConnectInfo :: GYNetworkId                              -- ^ The network identifier.
                                -> FilePath                                 -- ^ Path to the local node socket.
                                -> Api.LocalNodeConnectInfo Api.CardanoMode
networkIdToLocalNodeConnectInfo nid nodeSocket = Api.LocalNodeConnectInfo
    { localConsensusModeParams = Api.CardanoModeParams $ networkIdToEpochSlots nid
    , localNodeNetworkId       = toApiNetworkId nid
    , localNodeSocketPath      = nodeSocket
    }

-- | Constructs a Blockfrost client.
--
networkIdToProject :: GYNetworkId              -- ^ The network identifier.
                   -> String                   -- ^ The Blockfrost project identifier.
                   -> BlockfrostClient.Project
networkIdToProject nid pid = BlockfrostClient.Project
    { projectEnv = networkIdGYToBlockfrost nid
    , projectId  = T.pack pid
    }

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

-- |
--
-- >>> mapM_ LBS8.putStrLn $ Aeson.encode <$> [GYMainnet, GYTestnetPreprod, GYTestnetPreview, GYTestnetLegacy, GYPrivnet]
-- "mainnet"
-- "testnet-preprod"
-- "testnet-preview"
-- "testnet"
-- "privnet"
--
instance Aeson.ToJSON GYNetworkId where
    toJSON GYMainnet        = Aeson.toJSON ("mainnet" :: T.Text)
    toJSON GYTestnetPreprod = Aeson.toJSON ("testnet-preprod" :: T.Text)
    toJSON GYTestnetPreview = Aeson.toJSON ("testnet-preview" :: T.Text)
    toJSON GYTestnetLegacy  = Aeson.toJSON ("testnet" :: T.Text)
    toJSON GYPrivnet        = Aeson.toJSON ("privnet" :: T.Text)

    toEncoding GYMainnet        = Aeson.toEncoding ("mainnet" :: T.Text)
    toEncoding GYTestnetPreprod = Aeson.toEncoding ("testnet-preprod" :: T.Text)
    toEncoding GYTestnetPreview = Aeson.toEncoding ("testnet-preview" :: T.Text)
    toEncoding GYTestnetLegacy  = Aeson.toEncoding ("testnet" :: T.Text)
    toEncoding GYPrivnet        = Aeson.toEncoding ("privnet" :: T.Text)

-- |
--
-- >>> Aeson.eitherDecode @GYNetworkId <$> ["\"mainnet\"", "\"testnet-preprod\"", "\"testnet-preview\"", "\"testnet\"", "\"privnet\"", "\"no-such-net\""]
-- [Right GYMainnet,Right GYTestnetPreprod,Right GYTestnetPreview,Right GYTestnetLegacy,Right GYPrivnet,Left "Error in $: Expected mainnet, testnet-preprod, testnet-preview, testnet or privnet"]
--
instance Aeson.FromJSON GYNetworkId where
    parseJSON "mainnet"         = pure GYMainnet
    parseJSON "testnet-preprod" = pure GYTestnetPreprod
    parseJSON "testnet-preview" = pure GYTestnetPreview
    parseJSON "testnet"         = pure GYTestnetLegacy
    parseJSON "privnet"         = pure GYPrivnet
    parseJSON _                 = fail "Expected mainnet, testnet-preprod, testnet-preview, testnet or privnet"
