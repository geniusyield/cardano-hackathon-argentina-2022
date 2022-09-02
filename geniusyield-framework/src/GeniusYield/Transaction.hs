-- | Tools to build balanced transactions.
--
--Balancing algorithm.
--
-- Inputs:
--
-- * Transaction inputs
-- * Transaction outputs
-- * Transaction minted value
--
-- Additionally:
--
-- * Set of additional UTxOs which can be spent
-- * Collateral UTxO which shouldn't be spent
-- * Change address
--
-- The algorithm should produce sets of inputs and outputs
-- such the total value is @input + minted = outputs@.
--
-- Each output should be big enough
-- (contain enough ADA, 'Api.calculateMinimumUTxO').
-- Algorithm may adjust them to include additional value.
--
-- There are also transacton fees
-- which should also be taken into account.
-- We over-approximate the fees, and let
-- 'Api.makeTransactionBodyAutoBalance' balance fees.
-- (We can be more precise here, and only slightly over-approximate,
--  but @cardano-api@ doesn't provide a handy helpers to fill
--  in execution units).
--
-- The algorithm should add additional inputs so all inputs
-- have enough value for all (adjusted) outputs.
--
-- As usually inputs and outputs don't match equally,
-- the left-over inputs are sent to change address.
-- This output should also be adjusted.
-- (TODO: is it enough to send only non-ADA there?)
--
-- Collateral input is needed when scripts are executed,
-- i.e. transaction mints tokens or consumes script outputs.
--
-- See 'Api.evaluateTransactionBalance'
-- and 'Api.makeTransactionBodyAutoBalance'
-- (this function balances ADA only and doesn't add inputs, i.e. it calculates the ADA change).
--
module GeniusYield.Transaction (
    -- * IO interface
    buildUnsignedTxBody,
    BuildTxException (..),
    -- * Pure part of balancing
    pureBalanceTx,
    BalancingError (..),
) where

import qualified Cardano.Api                    as Api
import qualified Cardano.Api.Shelley            as Api.S
import qualified Data.Map                       as Map

import           GeniusYield.Types
import           GeniusYield.Imports

-------------------------------------------------------------------------------
-- IO Interface
-------------------------------------------------------------------------------

data BuildTxException
    = BuildTxBalancingError BalancingError
    | BuildTxQueryException String
    | BuildTxBodyErrorAutoBalance Api.TxBodyErrorAutoBalance
    | BuildTxUnknownNetwork
  deriving stock    (Show)
  deriving anyclass (Exception)

-- |
--
-- Throws 'BuildTxException'.
--
-- /Note/: this is mostly pure function, IO used for
--
-- * querying the node parameters (protocol, system start, etc.)
-- * querying utxos
--
buildUnsignedTxBody ::
           HasCallStack
        => GYProviders
        -> [GYTxIn]
        -> [GYTxOut]
        -> GYAddress                                         -- ^ change address
        -> GYTxOutRef                                        -- ^ collateral
        -> Maybe (GYValue, [(GYMintingPolicy, GYRedeemer)])  -- ^ minted values
        -> Maybe GYSlot
        -> Maybe GYSlot
        -> Set GYPubKeyHash
        -> IO GYTxBody
buildUnsignedTxBody providers insOld outsOld changeAddr collateral mmint lb ub signers = do
    ss                       <- gyGetSystemStart providers
    eh                       <- gyGetEraHistory providers
    pp                       <- gyGetProtocolParameters providers
    ps                       <- gyGetStakePools providers
    (ins, collaterals, outs) <- balanceTx providers pp insOld outsOld changeAddr collateral mmint
    utxos                    <- gyQueryUtxosAtTxOutRefs providers $ gyTxInTxOutRef <$> ins

    let outs' :: [Api.S.TxOut Api.S.CtxTx Api.S.AlonzoEra]
        outs' = txOutToApi <$> outs

        ins' :: [(Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxTxIn Api.AlonzoEra))]
        ins' = txInToApi <$> ins

        collaterals' :: Api.TxInsCollateral Api.AlonzoEra
        collaterals' = case collaterals of
            []    -> Api.TxInsCollateralNone
            orefs -> Api.TxInsCollateral Api.CollateralInAlonzoEra $ txOutRefToApi <$> orefs

        -- will be filled by makeTransactionBodyAutoBalance
        fee :: Api.TxFee Api.AlonzoEra
        fee = Api.TxFeeExplicit Api.TxFeesExplicitInAlonzoEra $ Api.Lovelace 0

        lb' :: Api.TxValidityLowerBound Api.AlonzoEra
        lb' = maybe
            Api.TxValidityNoLowerBound
            (Api.TxValidityLowerBound Api.ValidityLowerBoundInAlonzoEra . slotToApi)
            lb

        ub' :: Api.TxValidityUpperBound Api.AlonzoEra
        ub' = maybe
            (Api.TxValidityNoUpperBound Api.ValidityNoUpperBoundInAlonzoEra)
            (Api.TxValidityUpperBound Api.ValidityUpperBoundInAlonzoEra . slotToApi)
            ub

        extra :: Api.TxExtraKeyWitnesses Api.AlonzoEra
        extra = case toList signers of
            []   -> Api.TxExtraKeyWitnessesNone
            pkhs -> Api.TxExtraKeyWitnesses Api.ExtraKeyWitnessesInAlonzoEra $ pubKeyHashToApi <$> pkhs

        mint :: Api.TxMintValue Api.BuildTx Api.AlonzoEra
        mint = case mmint of
            Nothing      -> Api.TxMintNone
            Just (v, xs) -> Api.TxMintValue Api.MultiAssetInAlonzoEra (valueToApi v) $ Api.BuildTxWith $ Map.fromList
                [ ( mintingPolicyApiId p
                  , Api.PlutusScriptWitness
                        Api.PlutusScriptV1InAlonzo
                        Api.PlutusScriptV1
                        -- FIXME(VASIL): Reference script support.
                        (Api.S.PScript x)
                        Api.NoScriptDatumForMint (redeemerToApi r)
                        $ Api.ExecutionUnits 0 0
                )
                | (p, r) <- xs
                , let x = mintingPolicyToApi p
                ]

        body :: Api.TxBodyContent Api.BuildTx Api.AlonzoEra
        body = Api.TxBodyContent
            ins'
            collaterals'
            -- FIXME(VASIL): Reference script support.
            Api.TxInsReferenceNone
            outs'
            -- FIXME(VASIL): Return collateral mechanism support.
            Api.TxTotalCollateralNone
            Api.TxReturnCollateralNone
            fee
            (lb', ub')
            Api.TxMetadataNone
            Api.TxAuxScriptsNone
            extra
            (Api.BuildTxWith $ Just pp)
            Api.TxWithdrawalsNone
            Api.TxCertificatesNone
            Api.TxUpdateProposalNone
            mint
            Api.TxScriptValidityNone

    case Api.makeTransactionBodyAutoBalance
            Api.AlonzoEraInCardanoMode
            ss
            eh
            pp
            ps
            (utxosToApi utxos)
            body
            (addressToApi' changeAddr)
            Nothing of
        Left err                         -> throwIO (BuildTxBodyErrorAutoBalance err)
        Right (Api.BalancedTxBody b _ _) -> return (txBodyFromApi b)


balanceTx :: HasCallStack
          => GYProviders
          -> Api.S.ProtocolParameters
          -> [GYTxIn]
          -> [GYTxOut]
          -> GYAddress
          -> GYTxOutRef
          -> Maybe (GYValue, [(GYMintingPolicy, GYRedeemer)])
          -> IO ([GYTxIn], [GYTxOutRef], [GYTxOut])
balanceTx providers pp ins outs changeAddr collateral mmint = do
    utxosIn    <- gyQueryUtxosAtTxOutRefs providers $ gyTxInTxOutRef <$> ins
    utxosAddr' <- gyQueryUtxosAtAddress providers changeAddr

    either (throwIO . BuildTxBalancingError) pure $ pureBalanceTx pp ins utxosIn outs changeAddr utxosAddr' collateral mmint

-------------------------------------------------------------------------------
-- Pure part
-------------------------------------------------------------------------------

data BalancingError
    = BalacingErrorMinimumUTxO String         -- ^ failed to 'Api.calculateMinimumUTxO'.
    | BalancingErrorInsufficientFunds GYValue
  deriving stock    (Show)
  deriving anyclass (Exception)

minimumUTxO :: Api.S.ProtocolParameters -> GYTxOut -> Either BalancingError Natural
minimumUTxO pp txOut = do
    case Api.calculateMinimumUTxO Api.ShelleyBasedEraAlonzo (txOutToApi txOut) pp of
        Left err -> Left $ BalacingErrorMinimumUTxO $ show err
        Right v  -> return $ extractLovelace v

adjustTxOut :: Api.S.ProtocolParameters -> GYTxOut -> Either BalancingError GYTxOut
adjustTxOut pp  txOut = do
    needed <- minimumUTxO pp txOut
    let v         = gyTxOutValue txOut
        contained = extractLovelace $ valueToApi v
    if needed <= contained
        then return txOut
        else do
            let v'     = valueFromLovelace (fromIntegral $ needed - contained) <> v
                txOut' = txOut {gyTxOutValue = v'}
            adjustTxOut pp txOut'

pureBalanceTx :: HasCallStack
    => Api.S.ProtocolParameters -- ^ protocol parameters
    -> [GYTxIn]               -- ^ transaction inputs
    -> GYUTxOs                -- ^ utxos at inputs
    -> [GYTxOut]              -- ^ transaction outputs
    -> GYAddress              -- ^ change addr
    -> GYUTxOs                -- ^ utxos at the change addr
    -> GYTxOutRef             -- ^ collateral
    -> Maybe (GYValue, [(GYMintingPolicy, GYRedeemer)]) -- ^ minting
    -> Either BalancingError ([GYTxIn], [GYTxOutRef], [GYTxOut])
pureBalanceTx pp ins utxosIn outs changeAddr utxosAddr' collateral mmint = do
    let utxosAddr = utxosRemoveTxOutRef collateral utxosAddr'
    outs' <- mapM (adjustTxOut pp) outs
    let valueIn, valueMint, valueOut :: GYValue
        valueIn         = foldMapUTxOs utxoValue utxosIn
        valueMint       = maybe mempty fst mmint
        valueOut        = mconcat (gyTxOutValue <$> outs') <> valueFromLovelace 50_000_000 -- (hopefully) overestimating transaction fees!
        needsCollateral = valueMint /= mempty || any (isJust . gyTxInWitness) ins
        collaterals
            | needsCollateral = [collateral]
            | otherwise       = []
    let valueMissing    = missing (valueOut `valueMinus` (valueIn <> valueMint))
    (addIns, addVal) <- addInputs valueMissing utxosAddr $ gyTxInTxOutRef <$> ins
    let ins'        = ins ++ [GYTxIn x Nothing | x <- addIns]
        valueIn'    = valueIn <> addVal
        tokenChange = removeAda $ (valueIn' <> valueMint) `valueMinus` valueOut

    outs'' <- if tokenChange == mempty
        then return outs'
        else do
            out <- adjustTxOut pp $ GYTxOut changeAddr tokenChange Nothing
            return $ out : outs'

    return (ins', collaterals, outs'')
  where
    missing :: GYValue -> Map GYAssetClass Natural
    missing v = foldl' f Map.empty $ valueToList v
      where
        f :: Map GYAssetClass Natural -> (GYAssetClass, Integer) -> Map GYAssetClass Natural
        f m (ac, n)
            | n <= 0    = m
            | otherwise = Map.insert ac (fromIntegral n) m

    addInputs :: Map GYAssetClass Natural -> GYUTxOs -> [GYTxOutRef] -> Either BalancingError ([GYTxOutRef], GYValue)
    addInputs missing' utxos xs = go missing' [] mempty $ Map.keys valueMap'
      where
        valueMap' :: Map GYTxOutRef GYValue
        valueMap' = mapUTxOs utxoValue utxos

        go :: Map GYAssetClass Natural -> [GYTxOutRef] -> GYValue -> [GYTxOutRef] -> Either BalancingError ([GYTxOutRef], GYValue)
        go m addIns addVal _
            | Map.null m            = Right (addIns, addVal)
        go m _      _      []       = Left $ BalancingErrorInsufficientFunds $ valueFromList [ (ac, toInteger n) | (ac, n) <- Map.toList m ]
        go m addIns addVal (y : ys)
            | y `elem` xs           = go m addIns addVal ys
            | otherwise             =
                  let
                    v  = valueMap' Map.! y
                    m' = foldl' f m $ valueToList v
                      where
                        f :: Map GYAssetClass Natural -> (GYAssetClass, Integer) -> Map GYAssetClass Natural
                        f m'' (ac, n) =
                          let
                            o  = fromIntegral n
                          in
                            case Map.lookup ac m'' of
                                Nothing       -> m''
                                Just n'
                                    | n' <= o   -> Map.delete ac m''
                                    | otherwise -> Map.insert ac (n' - o) m''
                  in
                    if m' == m
                        then go m addIns addVal ys
                        else go m' (y : addIns) (addVal <> v) ys

    removeAda :: GYValue -> GYValue
    removeAda = snd . valueSplitAda

extractLovelace :: Api.Value -> Natural
extractLovelace v = case Api.selectLovelace v of Api.Lovelace n -> fromIntegral $ max 0 n
