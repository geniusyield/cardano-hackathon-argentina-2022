module GeniusYield.Providers
    ( module X
    , simpleProviders
    ) where

import GeniusYield.Providers.Blockfrost       as X
import GeniusYield.Providers.CachedQueryUTxOs as X
import GeniusYield.Providers.CardanoDbSync    as X
import GeniusYield.Providers.ChainIndex       as X
import GeniusYield.Providers.Katip            as X
import GeniusYield.Providers.LiteChainIndex   as X
import GeniusYield.Providers.Node             as X
import GeniusYield.Providers.SubmitApi        as X
import GeniusYield.Types

-- | Creates simples providers using a local node and either Blockfrost or a Chain Index.
--
simpleProviders :: GYEra                -- ^ Era in which local node operates
                -> GYNetworkId          -- ^ The network identifier.
                -> FilePath             -- ^ Path to the local node socket.
                -> Either String String -- ^ The Blockfrost project identifier or the Chain Index base Url.
                -> GYLog                -- ^ The logging provider.
                -> IO GYProviders
simpleProviders era nid nodeSocket e logging = do
    let info  = networkIdToLocalNodeConnectInfo nid nodeSocket
    ld <- case e of
        Left pid      -> return $ blockfrostLookupDatum $ networkIdToProject nid pid
        Right baseUrl -> chainIndexLookupDatum <$> newChainIndexEnv baseUrl
    return GYProviders
        { gyLookupDatum   = ld
        , gySubmitTx      = nodeSubmitTx info
        , gySlotActions   = nodeSlotActions info
        , gyGetParameters = nodeGetParameters era info
        , gyQueryUTxO     = nodeQueryUTxO era info
        , gyLog'          = logging
        }
