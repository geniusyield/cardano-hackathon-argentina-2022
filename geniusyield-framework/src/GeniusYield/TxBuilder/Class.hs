module GeniusYield.TxBuilder.Class
    ( MonadError (..)
    , MonadRandom (..)
    , GYTxMonadException (..)
    , GYTxMonad (..)
    , GYTxQueryMonad (..)
    , GYTxSkeleton (..)
    , RandT
    , scriptAddress
    , scriptAddress'
    , addressFromPlutus'
    , utxosDatums
    , utxoDatum
    , mustHaveInput
    , mustHaveOutput
    , mustHaveOptionalOutput
    , mustMint
    , mustBeSignedBy
    , isInvalidBefore
    , isInvalidAfter
    , gyLogDebug'
    , gyLogInfo'
    , gyLogWarning'
    , gyLogError'
    ) where

import           Control.Exception
import           Control.Monad.Except (MonadError (..))
import           Control.Monad.Random (MonadRandom (..), RandT, lift)
import           Data.List            (nubBy)

import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Plutus.V1.Ledger.Api as Plutus

import           GeniusYield.Imports
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- Exception
-------------------------------------------------------------------------------

newtype GYTxMonadException = GYTxMonadException String
    deriving (Show, Read, Eq, Ord)

instance IsString GYTxMonadException where
    fromString = GYTxMonadException

instance Exception GYTxMonadException

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Class of monads for querying chain data.
class MonadError GYTxMonadException m => GYTxQueryMonad m where
    -- | Get the network id
    networkId :: m GYNetworkId

    -- | Lookup datum by its hash.
    lookupDatum' :: GYDatumHash -> m (Maybe GYDatum)

    -- | Lookup 'GYUTxO' at 'GYTxOutRef'.
    --
    utxoAtTxOutRef' :: GYTxOutRef -> m (Maybe GYUTxO)

    -- | Lookup 'GYUTxOs' at multiple 'GYTxOutRef's at once
    utxosAtTxOutRefs' :: [GYTxOutRef] -> m GYUTxOs
    utxosAtTxOutRefs' orefs = utxosFromList <$> wither utxoAtTxOutRef' orefs

    -- | Lookup 'GYUTxOs' at 'GYAddress'.
    utxosAtAddress' :: GYAddress -> m GYUTxOs

    -- | Lookup the current 'GYSlot'.
    currentSlot' :: m GYSlot

    -- | Log a message with specified namespace and severity.
    log' :: HasCallStack => GYLogNamespace -> GYLogSeverity -> String -> m ()

-- | Class of monads for querying monads as a user.
class GYTxQueryMonad m => GYTxMonad m where
    -- FIXME(Vasil): Rename `ownPubKeyHash` - `Contarct.ownPaymentPubKeyHash` is deprecated.
    -- | Our public key hash
    ownPubKeyHash :: m GYPubKeyHash

    -- | Own addrss
    ownAddress    :: m GYAddress

    -- | Return some unspend transaction output
    --
    -- /Note:/ may or may not return the same value
    someUTxO :: m GYTxOutRef

instance GYTxQueryMonad m => GYTxQueryMonad (RandT g m) where
    networkId = lift networkId
    lookupDatum' = lift . lookupDatum'
    utxoAtTxOutRef' = lift . utxoAtTxOutRef'
    utxosAtTxOutRefs' = lift . utxosAtTxOutRefs'
    utxosAtAddress' = lift . utxosAtAddress'
    currentSlot' = lift currentSlot'
    log' ns s = lift . log' ns s

instance GYTxMonad m => GYTxMonad (RandT g m) where
    ownPubKeyHash = lift ownPubKeyHash
    ownAddress = lift ownAddress
    someUTxO = lift someUTxO

-------------------------------------------------------------------------------
-- Transaction skeleton
-------------------------------------------------------------------------------

-- | Transaction skeleton
--
-- /Note:/ let's add fields as we need them.
--
data GYTxSkeleton = GYTxSkeleton
    { gytxIns           :: ![GYTxIn]
    , gytxOuts          :: ![GYTxOut]
    , gytxMint          :: !(Map GYMintingPolicy (Map GYTokenName Integer, GYRedeemer))
    , gytxSigs          :: !(Set GYPubKeyHash)
    , gytxInvalidBefore :: !(Maybe GYSlot)
    , gytxInvalidAfter  :: !(Maybe GYSlot)
    } deriving Show

emptyGYTxSkeleton :: GYTxSkeleton
emptyGYTxSkeleton = GYTxSkeleton
    { gytxIns           = []
    , gytxOuts          = []
    , gytxMint          = mempty
    , gytxSigs          = mempty
    , gytxInvalidBefore = Nothing
    , gytxInvalidAfter  = Nothing
    }

instance Semigroup GYTxSkeleton where
    x <> y = GYTxSkeleton
        { gytxIns           = combineIns (gytxIns x) (gytxIns y)
        , gytxOuts          = gytxOuts x ++ gytxOuts y
        , gytxMint          = combineMint (gytxMint x) (gytxMint y)
        , gytxSigs          = Set.union (gytxSigs x) (gytxSigs y)
        , gytxInvalidBefore = combineInvalidBefore (gytxInvalidBefore x) (gytxInvalidBefore y)
        , gytxInvalidAfter  = combineInvalidAfter (gytxInvalidAfter x) (gytxInvalidAfter y)
        }
      where
        -- we keep only one input per utxo to spend
        combineIns u v = nubBy ((==) `on` gyTxInTxOutRef) (u ++ v)
        -- we cannot combine redeemers, so we just pick first.
        combineMint = Map.unionWith (\(amt, r) (amt', _r) -> (Map.unionWith (+) amt amt', r))

        combineInvalidBefore :: Maybe GYSlot -> Maybe GYSlot -> Maybe GYSlot
        combineInvalidBefore m        Nothing  = m
        combineInvalidBefore Nothing  n        = n
        combineInvalidBefore (Just s) (Just t) = Just (max s t)

        combineInvalidAfter :: Maybe GYSlot -> Maybe GYSlot -> Maybe GYSlot
        combineInvalidAfter m        Nothing  = m
        combineInvalidAfter Nothing  n        = n
        combineInvalidAfter (Just s) (Just t) = Just (min s t)

instance Monoid GYTxSkeleton where
    mempty = emptyGYTxSkeleton

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Calculate script's address.
scriptAddress :: GYTxQueryMonad m => GYValidator -> m GYAddress
scriptAddress v = do
    nid <- networkId
    return $ addressFromValidator nid v

-- | Calculate script's address.
scriptAddress' :: GYTxQueryMonad m => GYValidatorHash -> m GYAddress
scriptAddress' h = do
    nid <- networkId
    return $ addressFromValidatorHash nid h

-- | Create address from plutus.
addressFromPlutus' :: GYTxQueryMonad m => Plutus.Address -> m (Maybe GYAddress)
addressFromPlutus' addr = do
    nid <- networkId
    return $ addressFromPlutus nid addr

utxosDatums :: forall m a. (GYTxQueryMonad m, Plutus.FromData a) => GYUTxOs -> m (Map GYTxOutRef (GYAddress, GYValue, a))
utxosDatums = witherUTxOs utxoDatum

utxoDatum :: (GYTxQueryMonad m, Plutus.FromData a) => GYUTxO -> m (Maybe (GYAddress, GYValue, a))
utxoDatum utxo = case utxoDatumHash utxo of
    Nothing -> return Nothing
    Just h  -> do
        md <- lookupDatum' h
        return $ case md >>= Plutus.fromBuiltinData . datumToPlutus' of
            Nothing -> Nothing
            Just a  -> Just (utxoAddress utxo, utxoValue utxo, a)

mustHaveInput :: GYTxIn -> GYTxSkeleton
mustHaveInput i = emptyGYTxSkeleton {gytxIns = [i]}

mustHaveOutput :: GYTxOut -> GYTxSkeleton
mustHaveOutput o = emptyGYTxSkeleton {gytxOuts = [o]}

mustHaveOptionalOutput :: Maybe GYTxOut -> GYTxSkeleton
mustHaveOptionalOutput = maybe mempty $ \o -> emptyGYTxSkeleton {gytxOuts = [o]}

mustMint :: GYMintingPolicy -> GYRedeemer -> GYTokenName -> Integer -> GYTxSkeleton
mustMint p r tn n = emptyGYTxSkeleton {gytxMint = Map.singleton p (Map.singleton tn n, r)}

mustBeSignedBy :: GYPubKeyHash -> GYTxSkeleton
mustBeSignedBy pkh = emptyGYTxSkeleton {gytxSigs = Set.singleton pkh}

isInvalidBefore :: GYSlot -> GYTxSkeleton
isInvalidBefore s = emptyGYTxSkeleton {gytxInvalidBefore = Just s}

isInvalidAfter :: GYSlot -> GYTxSkeleton
isInvalidAfter s = emptyGYTxSkeleton {gytxInvalidAfter = Just s}

gyLogDebug', gyLogInfo', gyLogWarning', gyLogError' :: GYTxQueryMonad m => GYLogNamespace -> String -> m ()
gyLogDebug'   ns = log' ns GYDebug
gyLogInfo'    ns = log' ns GYInfo
gyLogWarning' ns = log' ns GYWarning
gyLogError'   ns = log' ns GYError
