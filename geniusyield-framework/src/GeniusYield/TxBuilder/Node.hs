module GeniusYield.TxBuilder.Node (
    GYTxMonadNode,
    runGYTxMonadNode,
    runGYTxMonadNodeC,
    runGYTxMonadNodeF,
) where

import           Control.Monad.IO.Class      (MonadIO (..))

import           GeniusYield.Transaction
import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Class
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- GY implementation
-------------------------------------------------------------------------------

-- | 'GYTxMonad' interpretation run against real node.
newtype GYTxMonadNode a = GYTxMonadNode { unGYTxMonadNode :: GYTxNodeEnv -> IO a }
  deriving stock (Functor)

type role GYTxMonadNode representational

instance Applicative GYTxMonadNode where
    pure x = GYTxMonadNode $ \_ -> return x
    (<*>) = ap

instance Monad GYTxMonadNode where
    m >>= k = GYTxMonadNode $ \env -> do
        x <- unGYTxMonadNode m env
        unGYTxMonadNode (k x) env

instance MonadIO GYTxMonadNode where
    liftIO = GYTxMonadNode . const

data GYTxNodeEnv = GYTxNodeEnv
    { envNid        :: GYNetworkId
    , envProviders  :: GYProviders
    , envAddr       :: GYAddress
    , envPkh        :: GYPubKeyHash
    , envCollateral :: GYTxOutRef
    }

instance MonadError GYTxMonadException GYTxMonadNode where
    throwError = liftIO . throwIO

    catchError action handler = GYTxMonadNode $ \env -> catch
        (unGYTxMonadNode action env)
        (\err -> unGYTxMonadNode (handler err) env)

instance GYTxQueryMonad GYTxMonadNode where
    networkId = GYTxMonadNode $ \env ->
        return $ envNid env

    lookupDatum' h = GYTxMonadNode $ \env ->
        gyLookupDatum (envProviders env) h

    utxosAtAddress' addr = GYTxMonadNode $ \env ->
        gyQueryUtxosAtAddress (envProviders env) addr

    utxoAtTxOutRef' oref = GYTxMonadNode $ \env ->
        gyQueryUtxoAtTxOutRef (envProviders env) oref

    utxosAtTxOutRefs' oref = GYTxMonadNode $ \env ->
        gyQueryUtxosAtTxOutRefs (envProviders env) oref

    currentSlot' = GYTxMonadNode $ \env ->
        gyGetCurrentSlot (envProviders env)

    log' ns s msg = GYTxMonadNode $ \env ->
        gyLog (envProviders env) ns s msg

instance GYTxMonad GYTxMonadNode where
    ownPubKeyHash = GYTxMonadNode $ return . envPkh
    ownAddress    = GYTxMonadNode $ return . envAddr

    someUTxO = do
        addr       <- ownAddress
        collateral <- getCollateral
        utxos      <- utxosAtAddress' addr
        case someTxOutRef $ utxosRemoveTxOutRef collateral utxos of
            Just (oref, _) -> return oref
            Nothing        -> throwError "someUTxO: no UTxO found at address"
      where
        getCollateral = GYTxMonadNode $ return . envCollateral

runGYTxMonadNode
    :: GYNetworkId
    ->  GYProviders
    -> GYAddress                           -- ^ our address
    -> GYTxOutRef                          -- ^ collateral
    -> GYTxMonadNode GYTxSkeleton
    -> IO GYTxBody
runGYTxMonadNode = coerce (runGYTxMonadNodeF @Identity)

runGYTxMonadNodeC
    :: forall a. GYNetworkId
    -> GYProviders
    -> GYAddress                            -- ^ our address
    -> GYTxOutRef                           -- ^ collateral
    -> GYTxMonadNode a
    -> IO a
runGYTxMonadNodeC = coerce (runGYTxMonadNodeF @(Const a))

runGYTxMonadNodeF
    :: forall f. Traversable f
    => GYNetworkId
    -> GYProviders
    -> GYAddress                            -- ^ our address
    -> GYTxOutRef                           -- ^ collateral
    -> GYTxMonadNode (f GYTxSkeleton)
    -> IO (f GYTxBody)
runGYTxMonadNodeF nid providers addr collateral (GYTxMonadNode action) = do
    pkh <- maybe
        (fail $ "addressToPubKeyHash " ++ show addr)
        return
        (addressToPubKeyHash addr)
    fbody <- action $ GYTxNodeEnv nid providers addr pkh collateral

    forM fbody $ \GYTxSkeleton {..} -> do
        let gytxMint' :: Maybe (GYValue, [(GYMintingPolicy, GYRedeemer)])
            gytxMint' | null gytxMint = Nothing
                | otherwise = Just
                ( valueFromList [ (GYToken (mintingPolicyId mp) tn, n) | (mp, (tokens, _)) <- itoList gytxMint, (tn, n) <- itoList tokens ]
                , [(mp, redeemer) | (mp, (_, redeemer)) <- itoList gytxMint]
                )

        buildUnsignedTxBody
            providers
            gytxIns
            gytxOuts
            addr
            collateral
            gytxMint'
            gytxInvalidBefore
            gytxInvalidAfter
            gytxSigs
