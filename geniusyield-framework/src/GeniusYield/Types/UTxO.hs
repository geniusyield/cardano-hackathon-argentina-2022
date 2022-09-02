module GeniusYield.Types.UTxO (
    GYUTxO (..),
    utxoFromApi,
    utxoFromApi',
    GYUTxOs,
    utxosFromApi,
    utxosToApi,
    utxosRemoveTxOutRef,
    someTxOutRef,
    -- * Filter and map
    filterUTxOs,
    mapMaybeUTxOs,
    mapUTxOs,
    witherUTxOs,
    -- * List conversions
    utxosFromList,
    utxosFromUTxO,
    utxosToList,
    -- * Folds
    foldlUTxOs',
    foldMapUTxOs,
    forUTxOs_,
    foldMUTxOs,
) where

import           GeniusYield.Imports

import qualified Cardano.Api                as Api
import qualified Cardano.Api.Shelley        as Api.S
import qualified Data.Map.Strict            as Map
import qualified Text.Printf                as Printf

import           GeniusYield.Types.Address
import           GeniusYield.Types.Datum
import           GeniusYield.Types.TxOutRef
import           GeniusYield.Types.Value

-- | An unspent transaction output.
--
data GYUTxO = GYUTxO
    { utxoRef       :: !GYTxOutRef
    , utxoAddress   :: !GYAddress
    , utxoValue     :: !GYValue
    , utxoDatumHash :: !(Maybe GYDatumHash)
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (ToJSON)

-- | A set of unspent transaction outputs.
--
-- Actually a map from unspent transaction outputs to address, value and datum hash.
--
newtype GYUTxOs = GYUTxOs (Map GYTxOutRef (GYAddress, GYValue, Maybe GYDatumHash))
  deriving (Eq, Show)

instance Semigroup GYUTxOs where
    GYUTxOs x <> GYUTxOs y = GYUTxOs (Map.union x y)

instance Monoid GYUTxOs where
    mempty = GYUTxOs mempty

utxosFromApi :: Api.UTxO era -> GYUTxOs
utxosFromApi (Api.UTxO m) = utxosFromList
    [ utxoFromApi' txIn out
    | (txIn, out) <- Map.toList m
    ]

utxosToApi :: GYUTxOs -> Api.UTxO Api.AlonzoEra
utxosToApi (GYUTxOs m) = Api.UTxO $ Map.foldlWithKey' f Map.empty m
  where
    f :: Map Api.TxIn (Api.TxOut Api.CtxUTxO Api.AlonzoEra)
      -> GYTxOutRef -> (GYAddress, GYValue, Maybe GYDatumHash)
      -> Map Api.TxIn (Api.TxOut Api.CtxUTxO Api.AlonzoEra)
    f m' oref out = Map.insert (txOutRefToApi oref) (g out) m'

    g :: (GYAddress, GYValue, Maybe GYDatumHash) -> Api.TxOut Api.CtxUTxO Api.AlonzoEra
    g (addr, v, md) = Api.TxOut
        (addressToApi' addr)
        (valueToApiTxOutValue v)
        (maybe Api.TxOutDatumNone (Api.TxOutDatumHash Api.ScriptDataInAlonzoEra . datumHashToApi) md)
        -- FIXME(VASIL): Reference script support.
        Api.S.ReferenceScriptNone

utxoFromApi :: Api.TxIn -> Api.TxOut Api.CtxTx Api.AlonzoEra -> GYUTxO
utxoFromApi txIn (Api.TxOut a v d _) = GYUTxO
    { utxoRef       = txOutRefFromApi txIn
    , utxoAddress   = addressFromApi' a
    , utxoValue     = valueFromApiTxOutValue v
    , utxoDatumHash = f d
    }
  where
    f :: Api.TxOutDatum Api.CtxTx Api.AlonzoEra -> Maybe GYDatumHash
    f Api.TxOutDatumNone          = Nothing
    f (Api.TxOutDatumHash _ hash) = Just $ datumHashFromApi hash
    f (Api.TxOutDatumInTx _ sd)   = Just $ hashDatum $ datumFromApi' sd
    f (Api.TxOutDatumInline _ sd) = Just $ hashDatum $ datumFromApi' sd

utxoFromApi' :: Api.TxIn -> Api.TxOut Api.CtxUTxO era -> GYUTxO
utxoFromApi' txIn (Api.TxOut a v d _) = GYUTxO
    { utxoRef       = txOutRefFromApi txIn
    , utxoAddress   = addressFromApi' a
    , utxoValue     = valueFromApiTxOutValue v
    , utxoDatumHash = f d
    }
  where
    f :: Api.TxOutDatum Api.CtxUTxO era -> Maybe GYDatumHash
    f Api.TxOutDatumNone          = Nothing
    f (Api.TxOutDatumHash _ hash) = Just $ datumHashFromApi hash
    f (Api.TxOutDatumInline _ sd) = Just $ hashDatum $ datumFromApi' sd

-- | Remove particular 'GYTxOutRef' from 'GYUTxOs'.
--
-- Used to remove collateral, so we don't use it in transactions.
--
utxosRemoveTxOutRef :: GYTxOutRef -> GYUTxOs -> GYUTxOs
utxosRemoveTxOutRef oref (GYUTxOs m) = GYUTxOs $ Map.delete oref m

-- | Get some output reference from 'GYUTxOs'.
--
-- Used to pick an input for minting, or selecting collateral (in tests).
--
someTxOutRef :: GYUTxOs -> Maybe (GYTxOutRef, GYUTxOs)
someTxOutRef (GYUTxOs m) = f <$> Map.minViewWithKey m where
    f ((oref, _), m') = (oref, GYUTxOs m')

-- | Filter 'GYUTxOs' with a predicate on 'GYUTxO'.
filterUTxOs :: (GYUTxO -> Bool) -> GYUTxOs -> GYUTxOs
filterUTxOs p (GYUTxOs m) = GYUTxOs $ Map.filterWithKey p' m where
    p' r (a, v, mh) = p $ GYUTxO r a v mh

-- | Map & filter 'GYUTxOs' contents.
mapMaybeUTxOs :: (GYUTxO -> Maybe a) -> GYUTxOs -> Map GYTxOutRef a
mapMaybeUTxOs p (GYUTxOs m) = Map.mapMaybeWithKey p' m where
    p' r (a, v, mh) = p $ GYUTxO r a v mh

-- | Map 'GYUTxOs' contents.
mapUTxOs :: (GYUTxO -> a) -> GYUTxOs -> Map GYTxOutRef a
mapUTxOs f = mapMaybeUTxOs $ Just . f

-- | Applicative version of 'mapMaybeUTxOs'.
witherUTxOs :: Applicative f => (GYUTxO -> f (Maybe a)) -> GYUTxOs -> f (Map GYTxOutRef a)
witherUTxOs f (GYUTxOs m) = iwither g m where
    g ref (a, v, mh) = f (GYUTxO ref a v mh)

utxosFromList :: [GYUTxO] -> GYUTxOs
utxosFromList xs = GYUTxOs $ Map.fromList
    [ (r, (a, v, mh))
    | GYUTxO r a v mh <- xs
    ]

utxosToList :: GYUTxOs -> [GYUTxO]
utxosToList (GYUTxOs m) = [GYUTxO r a v mh | (r, (a, v, mh)) <- Map.toList m]

utxosFromUTxO :: GYUTxO -> GYUTxOs
utxosFromUTxO utxo = utxosFromList [utxo]

foldlUTxOs' :: forall a. (a -> GYUTxO -> a) -> a -> GYUTxOs -> a
foldlUTxOs' f x (GYUTxOs m) = Map.foldlWithKey' f' x m
  where
    f' :: a -> GYTxOutRef -> (GYAddress, GYValue, Maybe GYDatumHash) -> a
    f' y r (a, v, mh) = f y $ GYUTxO r a v mh

foldMapUTxOs :: Monoid m => (GYUTxO -> m) -> GYUTxOs -> m
foldMapUTxOs f = foldlUTxOs' (\m utxo -> m <> f utxo) mempty

forUTxOs_ :: forall f a. Applicative f => GYUTxOs -> (GYUTxO -> f a) -> f ()
forUTxOs_ (GYUTxOs m) f = ifor_ m f'
  where
    f' :: GYTxOutRef -> (GYAddress, GYValue, Maybe GYDatumHash) -> f a
    f' r (a, v, mh) = f $ GYUTxO r a v mh

foldMUTxOs :: forall m a. Monad m => (a -> GYUTxO -> m a) -> a -> GYUTxOs -> m a
foldMUTxOs f x (GYUTxOs m) = foldM f' x $ Map.toList m
  where
    f' :: a -> (GYTxOutRef, (GYAddress, GYValue, Maybe GYDatumHash)) -> m a
    f' y (r, (a, v, mh)) = f y $ GYUTxO r a v mh

instance Printf.PrintfArg GYUTxOs where
    formatArg (GYUTxOs m) = Printf.formatArg $ unlines
        [ Printf.printf "%s %s" oref v
        | (oref, (_, v, _)) <- Map.toList m
        ]
