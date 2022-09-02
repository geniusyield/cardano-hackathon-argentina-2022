module GeniusYield.Types.SlotConfig (
    GYSlotConfig,
    slotToBeginPOSIXTime,
    slotToEndPOSIXTime,
    unsafeSlotFromPOSIXTime,
    makeSlotConfig,
    slotConfigFromPlutus
) where

import qualified Data.Time              as Time
import qualified Data.Time.Clock.POSIX  as Time
import qualified Ledger.TimeSlot        as Plutus

import           GeniusYield.Types.Time
import           GeniusYield.Types.Slot

--
-- $setup
--
-- >>> import           Data.Default          (Default (..))
-- >>> import qualified Data.Time.Clock.POSIX as Time
-- >>> import           GeniusYield.Types
--

data GYSlotConfig = GYSlotConfig
    { gySlotZeroTime :: !Time.POSIXTime
    , gySlotLength   :: !Time.NominalDiffTime
    }
  deriving Show

makeSlotConfig :: Time.UTCTime -> Time.NominalDiffTime -> GYSlotConfig
makeSlotConfig zero len = GYSlotConfig
    { gySlotZeroTime = Time.utcTimeToPOSIXSeconds zero
    , gySlotLength   = len
    }

-- | Get the starting 'GYTime' of a 'GYSlot' given a 'GYSlotConfig'.
--
-- >>> slotToBeginPOSIXTime (makeSlotConfig (Time.posixSecondsToUTCTime 10) 2) (unsafeSlotFromPlutus 1)
-- GYTime 12s
--
slotToBeginPOSIXTime :: GYSlotConfig -> GYSlot -> GYTime
slotToBeginPOSIXTime sc slot = timeFromPOSIX $ slotToBeginPOSIXTime' sc slot

slotToBeginPOSIXTime' :: GYSlotConfig -> GYSlot -> Time.POSIXTime
slotToBeginPOSIXTime' GYSlotConfig{..} slot =
    gySlotZeroTime + fromInteger (slotToInteger slot) * gySlotLength

-- | Get the ending 'GYTime' of a 'GYSlot' given a 'GYSlotConfig'.
--
-- >>> slotToEndPOSIXTime (makeSlotConfig (Time.posixSecondsToUTCTime 10) 2) (unsafeSlotFromPlutus 1)
-- GYTime 13.999s
--
slotToEndPOSIXTime :: GYSlotConfig -> GYSlot -> GYTime
slotToEndPOSIXTime sc@GYSlotConfig{..} slot =
    timeFromPOSIX $ slotToBeginPOSIXTime' sc slot + gySlotLength - oneMs
  where
    oneMs :: Time.NominalDiffTime
    oneMs = 0.001

-- | Get the 'GYSlot' of a 'GYTime' given a 'GYSlotConfig'.
--
-- >>> unsafeSlotFromPOSIXTime (makeSlotConfig (Time.posixSecondsToUTCTime 10) 2) (timeFromPOSIX 12)
-- GYSlot 1
--
-- >>> unsafeSlotFromPOSIXTime (makeSlotConfig (Time.posixSecondsToUTCTime 10) 2) (timeFromPOSIX 14)
-- GYSlot 2
--
unsafeSlotFromPOSIXTime :: GYSlotConfig -> GYTime -> GYSlot
unsafeSlotFromPOSIXTime GYSlotConfig{..} (timeToPOSIX -> time) =
    unsafeSlotFromPlutus $ floor $ (time - gySlotZeroTime) / gySlotLength

-- |
--
-- >>> slotConfigFromPlutus def
-- GYSlotConfig {gySlotZeroTime = 1596059091s, gySlotLength = 1s}
--
slotConfigFromPlutus :: Plutus.SlotConfig -> GYSlotConfig
slotConfigFromPlutus Plutus.SlotConfig {..} = GYSlotConfig
    { gySlotZeroTime = timeToPOSIX $ timeFromPlutus scSlotZeroTime
    , gySlotLength   = fromInteger scSlotLength / 1000
    }
