module GeniusYield.TxBuilder.Random
    ( MonadRandom (..)
    , withRandomness
    ) where

import qualified Cardano.Api                 as Api
import           Control.Monad.Random        (evalRandT, mkStdGen)
import qualified Data.ByteString             as BS

import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Class
import           GeniusYield.Types

-- | Convert a `GYTxMonad`-computation using randomness into a deterministic one
-- by using one's own public key hash as seed for the random number generator.
--
withRandomness :: GYTxMonad m => (forall n. (GYTxMonad n, MonadRandom n) => n a) -> m a
withRandomness x = do
    pkh <- ownPubKeyHash
    let seed = mkStdGen $ natToInt $ pubKeyHashToNat pkh
    evalRandT x seed

pubKeyHashToNat :: GYPubKeyHash -> Natural
pubKeyHashToNat = foldl' (\m w -> 256 * m + fromIntegral w) 0 . BS.unpack . Api.serialiseToRawBytes . pubKeyHashToApi

natToInt :: Natural -> Int
natToInt n = fromInteger $ minBoundInt + mod (fromIntegral n) (maxBoundInt - minBoundInt + 1)
  where
    minBoundInt, maxBoundInt :: Integer
    minBoundInt = fromIntegral (minBound :: Int)
    maxBoundInt = fromIntegral (maxBound :: Int)

