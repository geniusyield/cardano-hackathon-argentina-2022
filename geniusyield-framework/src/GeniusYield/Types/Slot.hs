module GeniusYield.Types.Slot (
    GYSlot,
    slotToApi,
    slotFromApi,
    slotToPlutus,
    unsafeSlotFromPlutus,
    advanceSlot,
    unsafeAdvanceSlot,
    slotToInteger,
) where

import           Data.Word           (Word64)
import           GeniusYield.Imports

import qualified Cardano.Api         as Api
import qualified Data.Aeson.Types    as Aeson
import qualified Ledger              as Plutus
import qualified Text.Printf         as Printf
import qualified Data.Swagger        as Swagger

newtype GYSlot = GYSlot Word64
  deriving (Show, Read, Eq, Ord)
  deriving newtype (Swagger.ToParamSchema, Swagger.ToSchema)

instance Printf.PrintfArg GYSlot where
    formatArg (GYSlot n) = Printf.formatArg (show n)

instance ToJSON GYSlot where
    toEncoding (GYSlot n) = Aeson.toEncoding n
    toJSON     (GYSlot n) = Aeson.toJSON n

slotToApi :: GYSlot -> Api.SlotNo
slotToApi = coerce

slotFromApi :: Api.SlotNo -> GYSlot
slotFromApi = coerce

slotToPlutus :: GYSlot -> Plutus.Slot
slotToPlutus = coerce (toInteger @Word64)

slotToInteger :: GYSlot -> Integer
slotToInteger = coerce (toInteger @Word64)

-- | Convert from plutus 'Plutus.Slot'.
--
-- As 'Plutus.Slot' is an 'Integer', it might under or overflow.
unsafeSlotFromPlutus :: HasCallStack => Plutus.Slot -> GYSlot
unsafeSlotFromPlutus (Plutus.Slot s)
    | s > toInteger (maxBound :: Word64) = error "slot overflow"
    | s < toInteger (minBound :: Word64) = error "slot underflow"
    | otherwise                          = GYSlot (fromInteger s)

-- | Advance 'GYSlot' forward. If slot value overflows, return 'Nothing'.
advanceSlot :: GYSlot -> Natural -> Maybe GYSlot
advanceSlot (GYSlot s) t
    | st > fromIntegral (maxBound :: Word64) = Nothing
    | otherwise                              = Just (GYSlot (fromIntegral st))
  where
    st :: Natural
    st = fromIntegral s + t

-- | Unsafe advance 'GYSlot'. Doesn't check for the overflow.
unsafeAdvanceSlot :: GYSlot -> Natural -> GYSlot
unsafeAdvanceSlot (GYSlot s) t = GYSlot (s + fromIntegral t)
