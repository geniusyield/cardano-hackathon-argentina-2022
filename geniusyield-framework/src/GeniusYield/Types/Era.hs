{-# LANGUAGE DeriveGeneric #-}

module GeniusYield.Types.Era (
    GYEra (..),
) where

import qualified Data.Aeson   as Aeson
import           GHC.Generics (Generic)

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson

-- | Eras at which cardano-node provider may operate.
--
-- We will drop the older eras when the transition to them is complete.
-- (atm, August 2022, we still need Alonzo a bit)
--
-- >>> Aeson.encode GYAlonzo
-- "\"GYAlonzo\""
--
-- >>> Aeson.decode @GYEra "\"GYBabbage\""
-- Just GYBabbage
--
data GYEra = GYAlonzo | GYBabbage
  deriving (Show, Read, Eq, Ord, Generic, Aeson.FromJSON, Aeson.ToJSON)
