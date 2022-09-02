module GeniusYield.TxBuilder.Contract
    ( GYTxMonadContract
    , asContract
    , submitGYTxMonadContract
    , getPOSIXTimeRange
    ) where

import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Trans.Class   (lift)
import           GeniusYield.Imports

import qualified Control.Lens                as L
import           Control.Monad.Except        (ExceptT (..), runExceptT)
import           Data.Default                (Default (..))
import qualified Data.Map.Strict             as Map
import           Data.Text                   (pack)
import qualified Ledger
import qualified Ledger.Constraints          as Constraints
import qualified Ledger.TimeSlot             as Ledger
import qualified Ledger.Tx                   as Tx
import qualified Plutus.Contract             as Contract
import qualified Plutus.V1.Ledger.Api        as Plutus
import qualified Plutus.V1.Ledger.Value      as Value

import           GeniusYield.TxBuilder.Class
import           GeniusYield.Types
import qualified GeniusYield.Types.Ada       as Ada

-------------------------------------------------------------------------------
-- Contract implementation
-------------------------------------------------------------------------------

-- | 'GYTxMonad' interpretation in 'Contract.Contract' monad.
newtype GYTxMonadContract w s e a = GYTxMonadContract (ReaderT GYTxEnvContract (ExceptT GYTxMonadException (Contract.Contract w s e)) a)
  deriving newtype (Functor, Applicative, Monad, MonadError GYTxMonadException)

instance Semigroup a => Semigroup (GYTxMonadContract w s e a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (GYTxMonadContract w s e a) where
    mempty = pure mempty

liftContract :: Contract.Contract w s e a -> GYTxMonadContract w s e a
liftContract = GYTxMonadContract . lift . lift

data GYTxEnvContract = GYTxEnvContract

asContract :: Contract.AsContractError e => GYTxMonadContract w s e a -> Contract.Contract w s e a
asContract (GYTxMonadContract action) = do
    e <- runExceptT $ runReaderT action GYTxEnvContract
    case e of
        Left (GYTxMonadException err) -> throwError $ L.review Contract._OtherContractError $ pack err
        Right a                       -> return a

instance Contract.AsContractError e => GYTxQueryMonad (GYTxMonadContract w s e) where
    networkId = return GYMainnet

    lookupDatum' h = liftContract $ do
        d <- Contract.datumFromHash $ datumHashToPlutus h
        return $ datumFromPlutus <$> d

    utxoAtTxOutRef' oref = liftContract $ do
        let oref' = txOutRefToPlutus oref
        mcout <- Contract.unspentTxOutFromRef oref'
        return $ join $ fromPlutusUtxoPiece . (oref',) <$> mcout

    utxosAtAddress' addr = liftContract $ do
        utxos <- Contract.utxosAt (addressToPlutus addr)
        return $ utxosFromList $ mapMaybe fromPlutusUtxoPiece $ Map.toList utxos

    -- it's safe to use unsafeSlotFromPlutus, chainState shouldn't overflow.
    currentSlot' = unsafeSlotFromPlutus <$> liftContract Contract.currentPABSlot

    log' ns s msg = liftContract $ case s of
        GYDebug   -> Contract.logDebug msg'
        GYInfo    -> Contract.logInfo  msg'
        GYWarning -> Contract.logWarn  msg'
        GYError   -> Contract.logError msg'
      where
        msg' = prettyNamespace ns <> ": " <> msg

instance Contract.AsContractError e => GYTxMonad (GYTxMonadContract w s e) where
    ownPubKeyHash = do
        pkh <- liftContract Contract.ownFirstPaymentPubKeyHash
        either (throwError . fromString . ("ownPubKeyHash: " ++)) return $ pubKeyHashFromPlutus (coerce @Ledger.PaymentPubKeyHash @Ledger.PubKeyHash pkh)

    ownAddress = addressFromPubKeyHash GYMainnet <$> ownPubKeyHash

    someUTxO = do
        pkh <- pubKeyHashToPlutus <$> ownPubKeyHash
        m1  <- liftContract $ getSomeTxOutRef pkh
        let m2 = m1 >>= txOutRefFromPlutus
        case m2 of
            Nothing  -> throwError "someUTxO: no UTxO found at address"
            Just ref -> return ref
      where
        getSomeTxOutRef :: Ledger.PubKeyHash -> Contract.Contract w s e (Maybe Tx.TxOutRef)
        getSomeTxOutRef pkh = do
            utxos <- Contract.utxosAt $ Ledger.Address (Plutus.PubKeyCredential pkh) Nothing
            return $ case Map.minViewWithKey utxos of
                Nothing                 -> Nothing
                Just ((txOutRef, _), _) -> Just txOutRef

fromPlutusUtxoPiece :: (Plutus.TxOutRef, Tx.ChainIndexTxOut) -> Maybe GYUTxO
fromPlutusUtxoPiece (oref, out) = do
    ref'  <- txOutRefFromPlutus oref
    addr' <- addressFromPlutus GYMainnet $ out L.^. Tx.ciTxOutAddress
    value' <- valueFromPlutus $ out L.^. Tx.ciTxOutValue
    return GYUTxO
        { utxoRef       = ref'
        , utxoAddress   = addr'
        , utxoValue     = value'
        , utxoDatumHash = out L.^? Tx.ciTxOutScriptDatum >>= datumHashFromPlutus . fst
        }

submitGYTxMonadContract :: Contract.AsContractError e => GYTxMonadContract w s e GYTxSkeleton -> Contract.Contract w s e Tx.CardanoTx
submitGYTxMonadContract action = do
    skeleton@GYTxSkeleton {..} <- asContract action

    let mintConstraints = mconcat
            [ Constraints.mustMintValueWithRedeemer
                (redeemerToPlutus redeemer)
                (valueToPlutus $ valueFromList
                    [ (GYToken (mintingPolicyId mp) tn, n)
                    | (tn, n) <- Map.toList tokens
                    ])
            | (mp, (tokens, redeemer)) <- Map.toList gytxMint
            ]

    let inputConstraints = foldMap inputConstraint gytxIns

    let outputConstraints = foldMap outputConstraint gytxOuts

    let signatureConstraints = mconcat [Constraints.mustBeSignedBy $ coerce $ pubKeyHashToPlutus pkh | pkh <- toList gytxSigs]

    unspentOutputs <- Map.fromList <$> wither
        (\(GYTxIn oref _) -> let oref' = txOutRefToPlutus oref in fmap (fmap (oref' ,)) (Contract.unspentTxOutFromRef oref'))
        gytxIns

    let otherScriptsAndData = mconcat
            [ Constraints.plutusV1OtherScript (validatorToPlutus v) <>
              Constraints.otherData (datumToPlutus d)
            | GYTxIn _ (Just (v, d, _)) <- gytxIns
            ]

    let timeConstraints = case getPOSIXTimeRange skeleton of
            Nothing -> mempty
            Just r  -> Constraints.mustValidateIn r

    let lookups     = mconcat $
            otherScriptsAndData :
            [ Constraints.plutusV1MintingPolicy (mintingPolicyToPlutus mp)
            | (mp, _) <- Map.toList gytxMint
            ] ++
            [ Constraints.unspentOutputs unspentOutputs
            ]

    Contract.submitTxConstraintsWith @Void
        lookups
        (inputConstraints <> outputConstraints <> mintConstraints <> signatureConstraints <> timeConstraints)
  where
    inputConstraint :: GYTxIn -> Constraints.TxConstraints Void Void
    inputConstraint i = case gyTxInWitness i of
        Nothing        -> Constraints.mustSpendPubKeyOutput ref
        Just (_, _, r) -> Constraints.mustSpendScriptOutput ref $ redeemerToPlutus r
      where
        ref = txOutRefToPlutus $ gyTxInTxOutRef i

    outputConstraint :: GYTxOut -> Constraints.TxConstraints Void Void
    outputConstraint (GYTxOut addr v md) = case Plutus.addressCredential addr' of
        Plutus.ScriptCredential vh  -> case md of
            Nothing -> mempty
            Just d  -> Constraints.mustPayToOtherScript vh (datumToPlutus d) value'
        Plutus.PubKeyCredential pkh -> case Plutus.addressStakingCredential addr' of
            Nothing                                               -> case md of
                Nothing -> Constraints.mustPayToPubKey (coerce pkh) value'
                Just d  -> Constraints.mustPayWithDatumToPubKey (coerce pkh) (datumToPlutus d) value'
            Just (Plutus.StakingHash (Plutus.PubKeyCredential h)) -> case md of
                Nothing -> Constraints.mustPayToPubKeyAddress (coerce pkh) (coerce h) value'
                Just d  -> Constraints.mustPayWithDatumToPubKeyAddress (coerce pkh) (coerce h) (datumToPlutus d) value'
            _                                                     -> mempty
      where
        addr' = addressToPlutus addr
        value = valueToPlutus v
        ada = Value.valueOf value Ada.adaSymbol Ada.adaToken

        minAda = 3_000_000

        value' | ada < minAda = value <> Ada.lovelaceValueOf (minAda - ada)
               | otherwise    = value

-- TODO: Don't use default SlotConfig
getPOSIXTimeRange :: GYTxSkeleton -> Maybe Plutus.POSIXTimeRange
getPOSIXTimeRange GYTxSkeleton {..} = case (gytxInvalidBefore, gytxInvalidAfter) of
    (Nothing, Nothing) -> Nothing
    (Just s, Nothing)  -> Just $ Plutus.from $ slotBeginning s
    (Nothing, Just t)  -> Just $ Plutus.to   $ slotEnd t
    (Just s, Just t)   -> Just $ Ledger.interval (slotBeginning s) (slotEnd t)
  where
    slotBeginning :: GYSlot -> Plutus.POSIXTime
    slotBeginning = Ledger.slotToBeginPOSIXTime def . slotToPlutus

    slotEnd :: GYSlot -> Plutus.POSIXTime
    slotEnd = Ledger.slotToEndPOSIXTime def . slotToPlutus
