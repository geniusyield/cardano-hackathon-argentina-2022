{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.DEX.Api.PartialOrder (
  PartialOrderDatum (..),
  PartialOrderAction (..),
  PartialOrderInfo (..),
  partialOrderValidator,
  partialOrderAddress,
  nftMintingPolicy,
  nftMintingPolicyId,
  partialOrderFee,
  minDeposit,
  partialOrders,
  placePartialOrder,
  completelyFillPartialOrder,
  partiallyFillPartialOrder,
  cancelPartialOrder,
  getPartialOrderInfo,
) where

import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Value as Plutus
import qualified PlutusTx
import qualified PlutusTx.Prelude as PlutusTx
import qualified PlutusTx.Ratio as PlutusTx

import GeniusYield.DEX.Api.Utils
import GeniusYield.Imports
import GeniusYield.TxBuilder.Class
import GeniusYield.Types

-------------------------------------------------------------------------------
-- partial order datum
-------------------------------------------------------------------------------

data PartialOrderDatum = PartialOrderDatum
  { -- | Public key hash of the owner. Order cancellations must be signed by this.
    podOwnerKey :: Plutus.PubKeyHash
  , -- | Address of the owner. Payments must be made to this address.
    podOwnerAddr :: Plutus.Address
  , -- | The asset being offered.
    podOfferedAsset :: Plutus.AssetClass
  , -- | The number of units being offered.
    podOfferedAmount :: PlutusTx.Integer
  , -- | The asset being asked for as payment.
    podAskedAsset :: Plutus.AssetClass
  , -- | The price for one unit of the offered asset.
    podPrice :: PlutusTx.Rational
  , -- | Minimal number of units of the offered asset that must be paid for in a partial filling.
    podMinFilling :: PlutusTx.Integer
  , -- | Token name of the NFT identifying this order.
    podNFT :: Plutus.TokenName
  }
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''PartialOrderDatum

-------------------------------------------------------------------------------
-- partial order action
-------------------------------------------------------------------------------

data PartialOrderAction
  = PartialCancel
  | PartialFill Integer
  | CompleteFill
  deriving stock (Show, Eq, Ord)

PlutusTx.makeIsDataIndexed ''PartialOrderAction [('PartialCancel, 0), ('PartialFill, 1), ('CompleteFill, 2)]

-------------------------------------------------------------------------------
-- partial order info
-------------------------------------------------------------------------------

data PartialOrderInfo = PartialOrderInfo
  { -- | Reference to the partial order.
    poiRef :: !GYTxOutRef
  , -- | Public key hash of the owner.
    poiOwnerKey :: !GYPubKeyHash
  , -- | Address of the owner.
    poiOwnerAddr :: !GYAddress
  , -- | The asset being offered.
    poiOfferedAsset :: !GYAssetClass
  , -- | The number of units being offered.
    poiOfferedAmount :: !Natural
  , -- | The asset being asked for as payment.
    poiAskedAsset :: !GYAssetClass
  , -- | The price for one unit of the offered asset.
    poiPrice :: !GYRational
  , -- | Minimal number of units of the offered asset that must be bought in a partial filling.
    poiMinFilling :: !Natural
  , -- | Token name of the NFT identifying this partial order.
    poiNFT :: !GYTokenName
  , -- | Number of lovelace included for fees and deposits.
    poiFeesDeposits :: !Natural
  }
  deriving stock (Show, Generic)

-------------------------------------------------------------------------------
-- constants
-------------------------------------------------------------------------------

partialOrderValidator :: GYValidator
partialOrderValidator = fromJust $ validatorFromCborHex "5908190100003232323232323232323232323232323232323232323232323232323232323232323232323232323222232323232323232323232325333026303430250031323232323233333028222533302c00112250011533302e3002302f00112230023031003133003002303000123375e00c605a605e6ea8004dd6181600211919192999817981e9998150008198018991919191919191919299981c1822801899299981c9981b11299981c8008a501533303b3375e607800200629444c008c0f4004dd6181d0078a99981c99b87008503e14985858dd49bae303901d13253330393047004132533303a3371090000008a99981d19b880010081533303a337126eb4c0ec06400454ccc0e8cc008c098ccc0bdc0180a80099816182499819a82019819a82025eb80dd4021a8018a99981d19981438933302f70201866605ee00cc0b001cdd400099816182499819a82019819a82025eb80dd419b80042043333302e304933033303b303d37540446606660766078607a6ea8088cc0ccc0ecc0f0c0f0c0f4dd5011198199ba833702010002660666076604a607a6ea8088cc0ccc0ecc0f0c094c0f4dd501119819981d9811981e9baa02233033303b303c3023303d375404497ae0303b303d375401a0160142930b0b0b0b0b1bad303a003153330393370e010a07c2a66607266002604a66605ce00c05001ccc0acc120cc0c940fccc0c940fd2f5c06ea0cdc099b813302300b304833032503f33032503f4bd7019811998158031ba8007304833032503f33032503f4bd70020a8010a4c2c2c604ce244cccc0ac044c0e006c02001cc0e4008c0d0004dd500d981a00b1bad303301433302c3756606401006a00a6eb0c0c4020dd6181818188028a4c6eacc0bcc0c0c0c4dd50009817181798181baa002375c605a605c0162c605860586058605800260566056002605400260546ea8c0a0014c09c008588c8c8c8c8c8c8c8c8c8cc084c0c0040dd4299981798011119b88303d00233704900000088008a99981798011119b87303d0023370460440080022002266e012002001301f0012330013370400466e0c01c00ccdc100119b83006003533302c303900414800054ccc0b0cdc480224000062290011998141112999817181d800880109980180099b86002001533302b3371200400220022004a66605666e24008004400840054ccc0a8cdc480128178981a8010801299981499b89002502e1303400210023034375a6052605400466e08008dd6981400098149baa3027006302700137546048604a002604a6ea8020c08c004c088004c084004c080004c07c004c078004c078dd50019180d180d1801800911998098011bae3019301b37540026eb8c064c068c06cdd50009180c180c180c180c00091980111919a99980b9aba3001122500112230020032223002374c006244a002660064a66602e66ebc005409c4894004488c00800cdd5800800911980911299980a80080f099199802980d980c80111980919bb03018301a0030010021001300230190010012322233301322253330170021330092330070243756603a002002264a6660300042a660286600e0020482660144660106eacc078004094c07000c4c8c8c94ccc074cdc78010008a9980b99805002001899803980f803180f8028a99980e99b9000200115330173300a004027133007301f00600515330173300a027003133007006301f005375c6036603a0086eb8c068c070010dd5980e980d8011bab301c301a002002001223330122225333016002133008233007008375a6038002002264a66602e0042a660266600e0020102660124660106eb4c074004024c06c00c4c8c8c94ccc070cdc78010008a9980b19805002001899803980f003180f0028a99980e19b9000200115330163300a00400b133007301e00600515330163300a00b003133007006301e005375c603460380086eb8c064c06c010dd6980e180d0011bad301b3019002002001480008c038894ccc0440045288a998069801980a00089801180a800918009119299980999b894800000c40044cdc0000a999809981019b860030021480005200233706004002446464a66602466ebc00d40880644cc014008dd31980280080198091809980a1baa003301130133754004446601066ec0008004064888c8cc0348894ccc04400440084c8c8cc0154ccc0554cc03ccdd7980b180b980b800803099baf3016001008133300a7000086eacc058c05cc060dd50010802180b801980b9baa001301400101a301e330083300c22533300f0011613253330123375e6026602800200c2602600226006602800460286ea8c0480040052f5c0444666008446e98ccc01888dd4198039bad002375a0026eac008dd5800801000919191119998021198020020008038010009180111980100100091198009129998068010800899998020031808001180880100091129998070008998048018010991919299980999baf00200113300c3376000466012602c00c602c00666601001600a602a0082a66602666e40dd70011bae00113300c006333300800a003301500400513300c003333300800a006005301500430110023010004301100122001220025740446660100040022940888cccc01000920002333300500248001d69bab0010032322223300622533300900110051533300b3375e6014601800200c26008601c601800226004601a0020026ea40048c8c0088cc0080080048c0088cc00800800555cfaab9d5734ae855d12ab9e4891cd6abee1cd55a4591b8cc812d25fc0e51e7eced956d06db5abbdaa5d70013006480084dd4a4410010034830236dc0520809bee024bd6f7b6301b8148000dc1240046e1d2000370e90011ba5480004dd424001"

partialOrderAddress :: GYTxQueryMonad m => m GYAddress
partialOrderAddress = scriptAddress partialOrderValidator

nftMintingPolicy :: GYMintingPolicy
nftMintingPolicy = fromJust $ mintingPolicyFromCborHex "59018401000032323232323232323232323232223232533300e301130090021533300e3300a23371290001bad30110013333008300600337566018600e60206ea8c030c040dd500190008b0b0a4c26464646464a6660280022c2a666026a660146601e466ebc014c048c058dd50009bac30113015375400a2a660146ae8cc04800454cc028c058dd6980a9808800899b8f375c601c60220020042930b199980618050039bab3010300b3014375400840022c6e48cdc59bad300f3010001375c601e60266ea8c03c004c048dd50009806801180618081baa003300f00137540044466601600400229408c8c94ccc02ccdc3a4000600c00426eb8c02400458c030004dd51803980418059baa001230073007300700123223300622533300c00112250011533300b3375e600c60120020082644460046eacc04000cc0240044c008c028004004dd4800aab9d2300222533300800114a02a66600e6006600a00229444c008c0180048c8c0088cc0080080048c0088cc0080080055d0aba25734aae7d55cf1b8748009"

nftMintingPolicyId :: GYMintingPolicyId
nftMintingPolicyId = "d6abee1cd55a4591b8cc812d25fc0e51e7eced956d06db5abbdaa5d7"

partialOrderFee, minDeposit :: Natural
partialOrderFee = 1_500_000
minDeposit = 3_000_000

-------------------------------------------------------------------------------
-- queries
-------------------------------------------------------------------------------

partialOrders :: GYTxQueryMonad m => (GYAssetClass -> Bool) -> m [PartialOrderInfo]
partialOrders assetFilter = do
  addr <- partialOrderAddress
  utxo <-
    filterUTxOs
      ( \GYUTxO {utxoValue} ->
          any
            (\(asc, x) -> assetFilter asc && x > 0)
            $ valueToList utxoValue
      )
      <$> utxosAtAddress' addr
  datums <- utxosDatums utxo
  Map.foldlWithKey'
    ( \accM oref vod -> do
        acc <- accM
        either (const acc) (: acc)
          <$> runExceptT (makePartialOrderInfo oref vod)
    )
    (pure [])
    datums

-------------------------------------------------------------------------------
-- tx construction
-------------------------------------------------------------------------------

placePartialOrder ::
  GYTxMonad m =>
  -- | Amount and asset to offer.
  (Natural, GYAssetClass) ->
  -- | The asset being asked for as payment.
  GYAssetClass ->
  -- | The price for one unit of the offered asset.
  GYRational ->
  -- | Minimal number of units of the offered asset that must be bought in a partial filling.
  Natural ->
  m GYTxSkeleton
placePartialOrder (offerAmt, offerAC) priceAC price minFilling = do
  when (offerAmt == 0) $ throwError "placePartialOrder: offered amount must be positive"
  when (price <= 0) $ throwError $ fromString $ "placePartialOrder: price must be positive, but is " <> show price
  when (minFilling == 0) $ throwError "placePartialOrder: min filling must be positive"

  pkh <- ownPubKeyHash
  addr <- ownAddress
  outAddr <- partialOrderAddress
  ref <- someUTxO

  let ref' = txOutRefToPlutus ref
  nft <-
    maybe
      (throwError $ fromString "placePartialOrder: invalid token name")
      return
      (expectedTokenName ref)

  let maxFills = ceiling $ toRational offerAmt / toRational minFilling
      nftV = valueSingleton (GYToken nftMintingPolicyId nft) 1
      offerAmt' = toInteger offerAmt
      offerV =
        valueSingleton offerAC offerAmt'
          <> nftV
          <> valueFromLovelace (toInteger $ maxFills * partialOrderFee + (maxFills - 1) * minDeposit)

      od =
        PartialOrderDatum
          { podOwnerKey = pubKeyHashToPlutus pkh
          , podOwnerAddr = addressToPlutus addr
          , podOfferedAsset = assetClassToPlutus offerAC
          , podOfferedAmount = offerAmt'
          , podAskedAsset = assetClassToPlutus priceAC
          , podPrice = rationalToPlutus price
          , podMinFilling = toInteger minFilling
          , podNFT = tokenNameToPlutus nft
          }

      o =
        GYTxOut
          { gyTxOutAddress = outAddr
          , gyTxOutDatum = Just $ GYDatum od
          , gyTxOutValue = offerV
          }

  return $
    mustHaveOutput o
      <> mustMint nftMintingPolicy (GYRedeemer $ Just ref') nft 1

-- | Completely fill a partially-fillable order.
completelyFillPartialOrder ::
  (HasCallStack, GYTxMonad m) =>
  -- | The order reference.
  GYTxOutRef ->
  m GYTxSkeleton
completelyFillPartialOrder orderRef = do
  oi@PartialOrderInfo {..} <- getPartialOrderInfo orderRef

  let price = partialOrderPrice oi poiOfferedAmount
      feesAndDeposits = valueFromLovelace $ max 0 $ toInteger poiFeesDeposits - toInteger partialOrderFee

  return $
    mustHaveInput (partialOrderInfoToIn oi CompleteFill)
      <> mustHaveOutput (partialOrderInfoToPayment oi $ price <> feesAndDeposits)
      <> mustMint nftMintingPolicy (GYRedeemer (Nothing :: Maybe Plutus.TxOutRef)) poiNFT (-1)

-- | Partially fill a partially-fillable order.
partiallyFillPartialOrder ::
  (HasCallStack, GYTxMonad m) =>
  -- | The order reference.
  GYTxOutRef ->
  -- | The amount of offered tokens to buy.
  Natural ->
  m GYTxSkeleton
partiallyFillPartialOrder orderRef amt = do
  oi@PartialOrderInfo {..} <- getPartialOrderInfo orderRef
  outAddr <- partialOrderAddress

  when (amt == 0) $
    throwError "partiallyFillPartialOrder: amount must be positive"
  when (amt >= poiOfferedAmount) $
    throwError $ fromString $ printf "partiallyFillPartialOrder: amount %d must be smaller than offered amount %d" amt poiOfferedAmount
  when (amt < poiMinFilling) $
    throwError $ fromString $ printf "partiallyFillPartialOrder: amount %d must not be smaller than min filling %d" amt poiMinFilling

  let od = partialOrderInfoToPartialOrderDatum oi {poiOfferedAmount = poiOfferedAmount - amt}
      price = partialOrderPrice oi amt
      feesAndDeposits = valueFromLovelace $ max 0 $ toInteger poiFeesDeposits - toInteger (partialOrderFee + minDeposit)
      v =
        valueSingleton poiOfferedAsset (toInteger $ poiOfferedAmount - amt)
          <> feesAndDeposits
          <> valueSingleton (GYToken nftMintingPolicyId poiNFT) 1
      payment = price <> valueFromLovelace (toInteger minDeposit)
      o =
        GYTxOut
          { gyTxOutAddress = outAddr
          , gyTxOutDatum = Just $ GYDatum od
          , gyTxOutValue = v
          }

  return $
    mustHaveInput (partialOrderInfoToIn oi $ PartialFill $ toInteger amt)
      <> mustHaveOutput o
      <> mustHaveOutput (partialOrderInfoToPayment oi payment)

cancelPartialOrder :: (HasCallStack, GYTxMonad m) => GYTxOutRef -> m GYTxSkeleton
cancelPartialOrder orderRef = do
  oi <- getPartialOrderInfo orderRef
  return $
    mustHaveInput (partialOrderInfoToIn oi PartialCancel)
      <> mustBeSignedBy (poiOwnerKey oi)
      <> mustMint nftMintingPolicy (GYRedeemer (Nothing :: Maybe Plutus.TxOutRef)) (poiNFT oi) (-1)

-------------------------------------------------------------------------------
-- utilities
-------------------------------------------------------------------------------

partialOrderInfoToPartialOrderDatum :: PartialOrderInfo -> PartialOrderDatum
partialOrderInfoToPartialOrderDatum PartialOrderInfo {..} =
  PartialOrderDatum
    { podOwnerKey = pubKeyHashToPlutus poiOwnerKey
    , podOwnerAddr = addressToPlutus poiOwnerAddr
    , podOfferedAsset = assetClassToPlutus poiOfferedAsset
    , podOfferedAmount = fromIntegral poiOfferedAmount
    , podAskedAsset = assetClassToPlutus poiAskedAsset
    , podPrice = PlutusTx.fromGHC $ toRational poiPrice
    , podMinFilling = fromIntegral poiMinFilling
    , podNFT = tokenNameToPlutus poiNFT
    }

partialOrderInfoToIn :: PartialOrderInfo -> PartialOrderAction -> GYTxIn
partialOrderInfoToIn oi@PartialOrderInfo {..} oa =
  GYTxIn
    { gyTxInTxOutRef = poiRef
    , gyTxInWitness = Just (partialOrderValidator, GYDatum $ partialOrderInfoToPartialOrderDatum oi, GYRedeemer oa)
    }

partialOrderInfoToPayment :: PartialOrderInfo -> GYValue -> GYTxOut
partialOrderInfoToPayment oi v =
  GYTxOut
    { gyTxOutAddress = poiOwnerAddr oi
    , gyTxOutValue = v
    , gyTxOutDatum = Just $ GYDatum $ txOutRefToPlutus $ poiRef oi
    }

partialOrderPrice :: PartialOrderInfo -> Natural -> GYValue
partialOrderPrice PartialOrderInfo {..} amt = valueSingleton poiAskedAsset $ ceiling $ rationalToGHC poiPrice * toRational amt

getPartialOrderInfo :: GYTxQueryMonad m => GYTxOutRef -> m PartialOrderInfo
getPartialOrderInfo oref = do
  res <- runExceptT $ do
    utxo <-
      maybeToExceptT (printf "unknown partial order reference %s" oref) $
        utxoAtTxOutRef' oref
    vod <-
      maybeToExceptT (printf "invalid partial order reference %s (wrong or unknown datum)" oref) $
        utxoDatum utxo
    makePartialOrderInfo oref vod
  either (throwError . GYTxMonadException) return res

makePartialOrderInfo :: GYTxQueryMonad m => GYTxOutRef -> (GYAddress, GYValue, PartialOrderDatum) -> ExceptT String m PartialOrderInfo
makePartialOrderInfo orderRef (_, v, PartialOrderDatum {..}) = do
  addr <- maybeToExceptT (errorMsg "invalid address") $ addressFromPlutus' podOwnerAddr

  key <- ExceptT $ return $ pubKeyHashFromPlutus podOwnerKey

  offeredAsset <- maybeToExceptT (errorMsg "invalid offered asset") $ return $ assetClassFromPlutus podOfferedAsset

  offeredAmount <-
    if podOfferedAmount <= 0
      then throwError $ errorMsg "negative offered amount"
      else
        if valueAssetClass v offeredAsset < podOfferedAmount
          then throwError $ errorMsg "insufficient value"
          else return $ fromInteger podOfferedAmount

  askedAsset <- maybeToExceptT (errorMsg "invalid asked-for asset") $ return $ assetClassFromPlutus podAskedAsset

  price <-
    let p = rationalFromPlutus podPrice
     in if p <= 0
          then throwError $ errorMsg $ "non-positive price " <> show p
          else return p

  minFilling <-
    if podMinFilling <= 0
      then throwError $ errorMsg $ "non-positive min filling " <> show podMinFilling
      else return $ fromInteger podMinFilling

  nft <- maybeToExceptT (errorMsg $ "invalid NFT token name " <> show podNFT) $ return $ tokenNameFromPlutus podNFT
  when (valueAssetClass v (GYToken nftMintingPolicyId nft) /= 1) $
    throwError "NFT not present"

  feesDeposits <- do
    let x = flip valueAssetClass GYLovelace $ v `valueMinus` valueSingleton offeredAsset (fromIntegral offeredAmount)
    if x < toInteger partialOrderFee
      then throwError $ errorMsg "insufficient fees and deposits"
      else return $ fromInteger x

  return
    PartialOrderInfo
      { poiRef = orderRef
      , poiOwnerKey = key
      , poiOwnerAddr = addr
      , poiOfferedAsset = offeredAsset
      , poiOfferedAmount = offeredAmount
      , poiAskedAsset = askedAsset
      , poiPrice = price
      , poiMinFilling = minFilling
      , poiNFT = nft
      , poiFeesDeposits = feesDeposits
      }
  where
    errorMsg :: String -> String
    errorMsg s = printf "invalid order reference %s (%s)" orderRef s
