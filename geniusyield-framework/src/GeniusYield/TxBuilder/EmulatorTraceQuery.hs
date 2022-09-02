module GeniusYield.TxBuilder.EmulatorTraceQuery (
    GYTxQueryMonadET,
    runGYTxQueryMonadET,
) where

import           Plutus.Trace.Emulator       (EmulatorTrace)

import qualified Control.Lens                as L
import qualified Control.Monad.Freer.Extras  as E
import qualified Data.Map.Strict             as Map
import qualified Ledger
import qualified Ledger.Index
import qualified Ledger.Blockchain
import qualified Plutus.Trace.Emulator       as E

import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Class
import           GeniusYield.Types

-- | We can query the chain state of 'EmulatorTrace'.
newtype GYTxQueryMonadET a = GYTxQueryMonadET (EmulatorTrace a)
  deriving newtype (Functor, Applicative, Monad)

runGYTxQueryMonadET :: GYTxQueryMonadET a -> EmulatorTrace a
runGYTxQueryMonadET = coerce

instance MonadError GYTxMonadException GYTxQueryMonadET where
    throwError e = GYTxQueryMonadET $ E.throwError $ E.GenericError $
        show e

    -- cannot catch.
    catchError action _ = action

instance GYTxQueryMonad GYTxQueryMonadET where
    networkId = return GYMainnet

    lookupDatum' h = GYTxQueryMonadET $ do
        chain <- L.view E.chainNewestFirst <$> E.chainState
        return $ fmap (\(Ledger.Datum d) -> GYDatum d) $ firstJust
            [ Map.lookup (datumHashToPlutus h) (Ledger.getCardanoTxData tx)
            | block <- chain
            , Ledger.Blockchain.Valid tx    <- block
            ]
      where
        firstJust :: [Maybe a] -> Maybe a
        firstJust [] = Nothing
        firstJust (Nothing : xs) = firstJust xs
        firstJust (x@(Just _) : _) = x

    utxosAtAddress' addr = GYTxQueryMonadET $ do
        utxoIndex <- Ledger.Index.getIndex . L.view E.index <$> E.chainState

        return $ utxosFromList
            $ filter (\utxo -> utxoAddress utxo == addr)
            $ mapMaybe (uncurry utxoFromPlutus)
            $ mapMaybe (\(oref, out) -> (\oref' -> (oref', out)) <$> txOutRefFromPlutus oref)
            $ Map.toList utxoIndex

    utxoAtTxOutRef' oref = GYTxQueryMonadET $ do
        utxoIndex <- Ledger.Index.getIndex . L.view E.index <$> E.chainState

        return $ case Map.lookup (txOutRefToPlutus oref) utxoIndex of
            Nothing    -> Nothing
            Just out   -> utxoFromPlutus oref out

    -- it's safe to use unsafeSlotFromPlutus, chainState shouldn't overflow.
    currentSlot' = GYTxQueryMonadET $ unsafeSlotFromPlutus . L.view E.currentSlot <$> E.chainState

    log' ns s msg = GYTxQueryMonadET $ case s of
        GYDebug   -> E.logDebug msg'
        GYInfo    -> E.logInfo  msg'
        GYWarning -> E.logWarn  msg'
        GYError   -> E.logError msg'
      where
        msg' = prettyNamespace ns <> ": " <> msg

utxoFromPlutus :: GYTxOutRef -> Ledger.TxOut -> Maybe GYUTxO
utxoFromPlutus oref Ledger.TxOut {..} = do
    addr <- addressFromPlutus GYMainnet txOutAddress
    val  <- valueFromPlutus txOutValue
    dh   <- traverse datumHashFromPlutus txOutDatumHash
    return $ GYUTxO oref addr val dh
