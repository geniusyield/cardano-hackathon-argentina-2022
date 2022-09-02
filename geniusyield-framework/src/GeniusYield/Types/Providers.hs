module GeniusYield.Types.Providers
    ( -- * Lookup Datum
      GYLookupDatum
      -- * Submit Tx
    , GYSubmitTx
      -- * Get current slot
    , GYSlotActions (..)
    , gyGetCurrentSlot
    , gyWaitForNextBlock
    , gyWaitForNextBlock_
    , gyWaitForNextBlockDefault
    , gyWaitUntilSlot
    , gyWaitUntilSlotDefault
      -- * Get protocol parameters
    , GYGetParameters (..)
    , gyGetProtocolParameters
    , gyGetSystemStart
    , gyGetEraHistory
    , gyGetStakePools
      -- * Query UTxO
    , GYQueryUTxO (..)
    , gyQueryUtxosAtAddress
    , gyQueryUtxosAtTxOutRefs
    , gyQueryUtxoAtTxOutRef
      -- * Logging
    , GYLog (..)
    , gyLog
    , gyLogDebug
    , gyLogInfo
    , gyLogWarning
    , gyLogError
    , noLogging
    , simpleConsoleLogging
      -- * Providers
    , GYProviders (..)
    ) where

import qualified Cardano.Api                 as Api
import qualified Cardano.Api.Shelley         as Api.S
import           Cardano.Slotting.Time       (SystemStart)
import           Control.Concurrent          (threadDelay)
import           Control.Monad.IO.Class      (MonadIO (..))
import           GeniusYield.Types.Address
import           GeniusYield.Types.Datum
import           GeniusYield.Types.Logging
import           GeniusYield.Types.Slot
import           GeniusYield.Types.Tx
import           GeniusYield.Types.TxOutRef
import           GeniusYield.Types.UTxO
import           GeniusYield.Imports

-------------------------------------------------------------------------------
-- All providers
-------------------------------------------------------------------------------

data GYProviders = GYProviders
    { gyLookupDatum   :: !GYLookupDatum
    , gySubmitTx      :: !GYSubmitTx
    , gySlotActions   :: !GYSlotActions
    , gyGetParameters :: !GYGetParameters
    , gyQueryUTxO     :: !GYQueryUTxO
    , gyLog'          :: !GYLog
    }

gyGetCurrentSlot :: GYProviders -> IO GYSlot
gyGetCurrentSlot = gyGetCurrentSlot' . gySlotActions

gyWaitForNextBlock :: GYProviders -> IO GYSlot
gyWaitForNextBlock = gyWaitForNextBlock' . gySlotActions

gyWaitUntilSlot :: GYProviders -> GYSlot -> IO GYSlot
gyWaitUntilSlot providers = gyWaitUntilSlot' (gySlotActions providers)

-- | 'gyWaitFroNextBlock' variant which doesn't return current slot.
--
gyWaitForNextBlock_ :: GYProviders -> IO ()
gyWaitForNextBlock_ = void . gyWaitForNextBlock' . gySlotActions

gyQueryUtxosAtAddress :: GYProviders -> GYAddress -> IO GYUTxOs
gyQueryUtxosAtAddress = gyQueryUtxosAtAddress' . gyQueryUTxO

gyQueryUtxosAtTxOutRefs :: GYProviders -> [GYTxOutRef] -> IO GYUTxOs
gyQueryUtxosAtTxOutRefs = gyQueryUtxosAtTxOutRefs' . gyQueryUTxO

gyQueryUtxoAtTxOutRef :: GYProviders -> GYTxOutRef -> IO (Maybe GYUTxO)
gyQueryUtxoAtTxOutRef = gyQueryUtxoAtTxOutRef' . gyQueryUTxO

gyGetProtocolParameters :: GYProviders -> IO Api.S.ProtocolParameters
gyGetProtocolParameters = gyGetProtocolParameters' . gyGetParameters

gyGetSystemStart :: GYProviders -> IO SystemStart
gyGetSystemStart = gyGetSystemStart' . gyGetParameters

gyGetEraHistory :: GYProviders -> IO (Api.EraHistory Api.CardanoMode)
gyGetEraHistory = gyGetEraHistory' . gyGetParameters

gyGetStakePools :: GYProviders -> IO (Set Api.S.PoolId)
gyGetStakePools = gyGetStakePools' . gyGetParameters

-------------------------------------------------------------------------------
-- Lookup datum
-------------------------------------------------------------------------------

-- | How to query a datum by its hash?
type GYLookupDatum = GYDatumHash -> IO (Maybe GYDatum)

-------------------------------------------------------------------------------
-- Submit tx
-------------------------------------------------------------------------------

-- | How to submit a transaction?
type GYSubmitTx = GYTx -> IO GYTxId

-------------------------------------------------------------------------------
-- Current slot
-------------------------------------------------------------------------------

-- | How to get current slot?
data GYSlotActions = GYSlotActions
    { gyGetCurrentSlot'   :: !(IO GYSlot)
    , gyWaitForNextBlock' :: !(IO GYSlot)
    , gyWaitUntilSlot'    :: !(GYSlot -> IO GYSlot)
    }

-- | Wait for the next block
--
-- 'threadDelay' until current slot getter returns another value.
gyWaitForNextBlockDefault :: IO GYSlot -> IO GYSlot
gyWaitForNextBlockDefault getCurrentSlot = do
    s <- getCurrentSlot
    go s
  where
    go :: GYSlot -> IO GYSlot
    go s = do
        threadDelay 100_000
        t <- getCurrentSlot
        if t > s
            then return t
            else go s

-- | Wait until slot.
--
-- Returns the new current slot, which might be larger.
gyWaitUntilSlotDefault :: IO GYSlot -> GYSlot -> IO GYSlot
gyWaitUntilSlotDefault getCurrentSlot s = loop
  where
    loop :: IO GYSlot
    loop = do
        t <- getCurrentSlot
        if t >= s
            then return t
            else do
                threadDelay 100_000
                loop

-------------------------------------------------------------------------------
-- Protocol parameters
-------------------------------------------------------------------------------

-- | How to get protocol parameters? ... and other data to do balancing.
data GYGetParameters = GYGetParameters
    { gyGetProtocolParameters' :: !(IO Api.S.ProtocolParameters)
    , gyGetSystemStart'        :: !(IO SystemStart)
    , gyGetEraHistory'         :: !(IO (Api.EraHistory Api.CardanoMode))
    , gyGetStakePools'         :: !(IO (Set Api.S.PoolId))
    }

-------------------------------------------------------------------------------
-- Query UTxO
-------------------------------------------------------------------------------

-- | How to query utxos?
data GYQueryUTxO = GYQueryUTxO
    { gyQueryUtxosAtAddress'   :: !(GYAddress -> IO GYUTxOs)
    , gyQueryUtxosAtTxOutRefs' :: !([GYTxOutRef] -> IO GYUTxOs)
    , gyQueryUtxoAtTxOutRef'   :: !(GYTxOutRef -> IO (Maybe GYUTxO))
    }

-------------------------------------------------------------------------------
-- Logging
-------------------------------------------------------------------------------

data GYLog = GYLog
    { logRun     :: HasCallStack => GYLogNamespace -> GYLogSeverity -> String -> IO ()
    , logCleanUp :: IO ()
    }

gyLog :: (HasCallStack, MonadIO m) => GYProviders -> GYLogNamespace -> GYLogSeverity -> String -> m ()
gyLog providers ns s = liftIO . logRun (gyLog' providers) ns s

gyLogDebug, gyLogInfo, gyLogWarning, gyLogError :: (HasCallStack, MonadIO m) => GYProviders -> GYLogNamespace -> String -> m ()
gyLogDebug   p ns = gyLog p ns GYDebug
gyLogInfo    p ns = gyLog p ns GYInfo
gyLogWarning p ns = gyLog p ns GYWarning
gyLogError   p ns = gyLog p ns GYError

noLogging :: GYLog
noLogging = GYLog
    { logRun     = \_ns _s _msg -> return ()
    , logCleanUp = return ()
    }

simpleConsoleLogging
    :: (String -> IO ())   -- ^ putStrLn variant
    -> GYLog
simpleConsoleLogging f = GYLog
    { logRun     = \ns _s msg -> f $ printf "*** [%s] %s" ns msg
    , logCleanUp = return ()
    }
