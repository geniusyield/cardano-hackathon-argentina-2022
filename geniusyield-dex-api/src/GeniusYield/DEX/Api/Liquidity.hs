{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.DEX.Api.Liquidity (
  LiquidityDatum (..),
  LiquidityAction (..),
  gyPubKeyHash,
  gyAddress,
  gyFee,
  liquidityValidator,
  PoolInfo (..),
  SwapDirection (..),
  listPools,
  openPool,
  updatePool,
  swapPool,
  collectPool,
  closePool,

  -- * Used by bots
  swap',
  calculateSwap,

  -- * helper functions used in testing
  poolInfoToIn,
  getPoolInfo,
  poolAddr',
  ratToPair,
) where

import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified PlutusTx as PlutusTx
import qualified PlutusTx.Prelude as PlutusTx
import PlutusTx.Ratio (fromGHC, toGHC)
import qualified PlutusTx.Ratio as PlutusTx

import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Value as Plutus

import GeniusYield.DEX.Api.PartialOrder (nftMintingPolicy, nftMintingPolicyId)
import GeniusYield.DEX.Api.Utils (expectedTokenName)
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types

-------------------------------------------------------------------------------
-- liquidity datum
-------------------------------------------------------------------------------

-- | The datum attached to a liquidity position.
data LiquidityDatum = LiquidityDatum
  { -- | Pubkey hash of the owner (i.e. the liquidity provider).
    ldOwner :: !Plutus.PubKeyHash
  , -- | Token name of the NFT tracking the identity of the liquidity position.
    ldNFT :: !Plutus.TokenName
  , -- | One of the tokens in the position.
    ldFrom :: !Plutus.AssetClass
  , -- | The other token in the position.
    ldTo :: !Plutus.AssetClass
  , -- | The price for one unit of the "from" token in terms of the "to" token, given as numerator and denominator of the actual rational price.
    ldPrice :: !(PlutusTx.Integer, PlutusTx.Integer)
  , -- | The fee due for each swap, given as numerator and denominator of the actual rational proportion.
    ldFee :: !(PlutusTx.Integer, PlutusTx.Integer)
  , -- | GeniusYield fees in terms of the "from" token contained in the position.
    ldGYFeeFrom :: !PlutusTx.Integer
  , -- | GeniusYield fees in terms of the "to" token contained in the position.
    ldGYFeeTo :: !PlutusTx.Integer
  , -- | Minimal amount of "from" tokens that need to be swapped.
    ldMinFrom :: !PlutusTx.Integer
  }
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''LiquidityDatum

-------------------------------------------------------------------------------
-- liquidity action
-------------------------------------------------------------------------------

-- | Possible redeemers for consuming a liquidity position.
data LiquidityAction
  = -- | Perform a swap.
    Swap
  | -- | Collect the GeniusYield fees.
    CollectFees
  | -- | Update the parameters of the position.
    Update
  | -- | Close the position.
    Close
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''LiquidityAction

-------------------------------------------------------------------------------
-- constants
-------------------------------------------------------------------------------

-- | The GeniusYield pubkey hash.
gyPubKeyHash :: GYPubKeyHash
gyPubKeyHash = "df1e21b8463b4b8bca186492d4c77aa400ed62c56af0d726f5c2f8b6"

-- | The GeniusYield address on the preprod-testnet.
gyAddress :: GYAddress
gyAddress = addressFromTextUnsafe "addr_test1vr03ugdcgca5hz72rpjf94x802jqpmtzc440p4ex7hp03dsx6yptw"

-- | The GeniusYield fee.
gyFee :: GYRational
gyFee = rationalFromGHC $ 1 / 20

-- | The liquidity position validator on the preprod testnet.
liquidityValidator :: GYValidator
liquidityValidator = fromJust $ validatorFromCborHex "590e300100003232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323223232223232325333033303c002132323232533303730403301d3756606a606c605e6ea80080044c8c8c8c94ccc0eccdd79819802182319816981c98199baa0103302d3039303a303337540206605a60726074607460666ea8040cc0b4c0e4c0a0c0ccdd500819816981c981d181418199baa0103302d30393027303337540206605aa08e6605aa08e6605a60726074604c60666ea80412f5c02a6660766605c91011cdf1e21b8463b4b8bca186492d4c77aa400ed62c56af0d726f5c2f8b60037586072604c60666ea802054ccc0ecccc0bdc4801199814b800010031498585858ccc0a1c09bab3038303930323754606600600466604ee04dd5981b981c18189baa004001302000d33301b0030010041498c104cc0a140e0cc0a0dd49bae30343035302e375401697ae03301800137586066605a6ea8008c058014c0c4c0acdd50020a99981999b87480100084c8c8c94ccc0d8c0fccc070dd5981a181a98171baa3301900237586068605c6ea800c0044c8c8c94ccc0e4cc0b0dd7181b98189baa00e3758606e604860626ea801854ccc0e4c06800454ccc0e4cdd7981b981c18189baa002303730383031375401c2a66607266ebcc0dcc0e0c0e0c0c4dd5001181b981c181c18189baa00e153330393375e606e604c60626ea8008c0dcc098c0c4dd50070a99981c9980c01e180b980b181b981c181318189baa002153330393301903c3017301630373025303137540042a66607266ebcc0dcc0e0c094c0c4dd5001181b981c181298189baa00e153330393375e606e604860626ea8008c0dcc090c0c4dd50070a4c2c2c2c2c2c2c2c2c2c66604ce04dd5981b181b98181baa3031002302000d302f00133301a0020010031498c100cc09d40dccc09cdd49bae30333034302d375401497ae030160053031302b37540082a66606666e1d200600213232325333036303f3301c37566068606a605c6ea8cc064008dd6181a18171baa00300113253330373302a375c606a605e6ea8030dd6181a981118179baa00415333037533013333034222533303b0021533303b00114a2294054ccc0ec0045280a99981d29981899baf303330380023033303800113375e60646070004606460700022660066072004607200229400040f44c8ccc0b1c48011981a91299981d80081f899299981da9981919baf3039303a303a00100413375e6072002980129d8799fd8799f581cdf1e21b8463b4b8bca186492d4c77aa400ed62c56af0d726f5c2f8b6ffd87a80ff00137566072607460666ea8c0e40084c00cc0e8008c0c8dd5181c0009bac303630373030375400a6084660526606844a6660740022c264a66607466ebcc0e0c0e40040184c0e00044c00cc0e4008c0c4dd5181b8009bac303530363022302f375400897ae0153330373370e6603a6eacc0d4c090c0bcdd5002001281d8a4c2c2c2c603c016293182019813a81b998139ba9375c60666068605a6ea80292f5c0602c00a606260566ea80104c8c8c8c94ccc0dcc100cc074dd5981a981b18179baa0020011323232323232323232323232333333333304022222222533304b337100080042a66609666e2000c0044c8c8c94ccc138cdc499b810063302700630293302630253370200600401400824460046604c6ea000d41704894004cc098004c09ccc094c09000401ccc094004c098cc090c08c00401ccdc08010020991919299982719b893370200c6604e00c60526604c604a66e0400c008028010488c008cc098dd4001a82e091280099813000981399812981200080399812800981319812181180080399b810020041533304b337100060022666666666010600e44a66609a66609a60aa004941288a99982699b880024800048cc004c154008c15400c48cc00400800c5801801400c010004008888c008cc090c130c134c118dd5001982618231baa00312250011225001016017233001480092028004003002001232325333046302700d15333046302700c1533304633025049019153330463302604901a15333046001153330463375e01c0042930b0b0b0b0b0b29981099b893370066e0002401801ccdc01bad304330443031303d375401a008266e24cdc019b80008005301f3301d301c007018337006eb4c10cc0c0c0f4dd500680198279981b1821181e1baa0193303630423043303c37540326606c60846086608660786ea8064cc0d8c108c0c4c0f0dd500c9981b182118219818981e1baa0193303630423030303c37540326606c6ea0cdc00041bad3042303c37540026606c6ea0cdc00039bad30423043303c37540026606c60846086605e60786ea80652f5c02c6605000e6080605e60746ea805ccc09c018c0fcc100c100c0e4dd500b19813003181f1816981c1baa01533025005303d303e303e303737540286eb4c0f0c0f4c0a4c0d8dd50099bad303b3028303537540246eb4c0e8c0ecc0a0c0d0dd5008999814b8137566072607460666ea8c0d0010008ccc0a1c09bab303830393032375400a002604201c6060002666036006002008293182099814281c198141ba9375c6068606a605c6ea802d2f5c0660300026eb0c0ccc0b4dd5001180b002981898159baa004302a002302a001375400460166014605660586034604a6ea8008c028c024c0a8c060c090dd500091181b9980f0011980f000a5eb8088cc00400920022233002223003222330013370400a00666e0801000888c8c8c8c88cc004cdc100119b830070033370400466e0c01800d4ccc0bcc0dc010520001533302f3371200890000198a400466050a66605c66e2400800440044008cc01c0080054ccc0b4cdc480128188981a0010801299981619b8900250301303300210022253330293371200400220042002464a6660506600e00405620022a66605060044466e1ccdc12400400466e08c01401000440044cdc0240040026004002460024464a66605260660062002266e000054ccc0a4c0c4cdc30018010a40002900119b8300200123302500114a246466ec0c08c004c08cc090004c070dd500091198009bad301d002375a60380044460044460064466e20cdc100080219b82002003223002223003223371266e08004010cdc100100191980b11980b918161bad301a0013756603200200246464a666042605460340042603e0022c60300026ea8c074c078c05cdd5000911980e1129998110008b0a99981099baf004301f30193754603e0022603e604060326ea8c07c0044c008c080004004888c94ccc080c0a4c06c0044c8c8c94ccc08cc0b0c070008584c8cccccc0808cdc78011bae3023301d37540026eb0c088c08cc03cc070dd5003111180118129813180f9baa003122500137600082c6eb8c084004c068004dd5180f98101810180c9baa001301e001163300423029330063756603c603e60306ea800400cc8c8cccc074c004014dd6180f980c9baa00323300730023020301a37546040604260346ea800400c5888cdd78011810180d1baa0013758603a603c602e6ea80048c064894ccc07c00452f5c0264a66603e60080022660220026006603c00426006603c0046038002446466660086eb8c06cc054dd5001240004666600a00490003ad37560020066eb8c068c06cc050dd50009191111980d91299981080088028a99981019baf3019301e001006130043018301e00113002301f0010013752002466600ee00cc008c05cc060c060c044dd5000980b980c180298089baa001330023017300630113754002602e600860226ea800488c8c94ccc06ccdd7801a8138100998028011ba6330050010033018301930123754006602e60226ea800888cc028cdd800100080f1180a980a98010009180a180a18010009180998099809800911199802111ba63330062237506600e6eb4008dd68009bab00237560020040024646444666600846600800800200e00400246004466004004002446600244a66602e00420022666600800c6028004602a004002444a6660300022660120060042646464a66603466ebc0080044cc030cdd80011980498090031809001999804005802980c8020a99980d19b90375c0046eb80044cc030018cccc02002800cc0640100144cc03000ccccc020028018014c064010c048008c044010c05400488004880095d01119198069129998098008a50153330123375e602000200629444c008c044004008dd480111911199806911299980a00109980491980380d1bab300c0010011325333015002153300b3300700101a13300a2330083756601a00203660260062646464a66602e66e3c00800454cc038cc02801000c4cc01cc058018c05801454ccc05ccdc80010008a998071980500200e899803980b0030028a998071980500e801899803803180b0029bae300f3014004375c601c60260086eacc030c048008dd5980598088010010009119980611129998098010998041198038041bad300b0010011325333014002153300a33007001008133009233008375a601800201260240062646464a66602c66e3c00800454cc034cc02801000c4cc01cc054018c05401454ccc058cdc80010008a9980699805002005899803980a8030028a9980699805005801899803803180a8029bae300e3013004375c601a60240086eb4c02cc044008dd698051808001001000a40004601044a66601c002294454cc010c00cc02c0044c008c03000488ccc02c0080045282ab9e5573a6006444a6660126022002200426600600266e18008004cc0088894ccc02400440084cc00ccdc000124004600e00290001800911299980400089128008a99980398011802800891180118038018998018011803000919180111980100100091801119801001000aba15744ae6955cf89ba94891cd6abee1cd55a4591b8cc812d25fc0e51e7eced956d06db5abbdaa5d700233001480012002130034800840052f5bded8c06e052000370e90001b8748008dc4a40006e9520001375090001"

-------------------------------------------------------------------------------
-- Pool info
-------------------------------------------------------------------------------

data PoolInfo = PoolInfo
  { piRef :: !GYTxOutRef
  , piOwner :: !GYPubKeyHash
  , piNFT :: !GYTokenName
  , piFrom :: !GYAssetClass
  , piTo :: !GYAssetClass
  , piAmtFrom :: !Natural
  , piAmtTo :: !Natural
  , piGYFeeFrom :: !Natural
  , piGYFeeTo :: !Natural
  , piPrice :: !GYRational
  , piFee :: !GYRational
  , piMinFrom :: !Integer
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

poolInfoToIn :: PoolInfo -> LiquidityAction -> (GYTxIn, LiquidityDatum)
poolInfoToIn pi' la =
  ( GYTxIn
      { gyTxInTxOutRef = piRef pi'
      , gyTxInWitness = Just (liquidityValidator, GYDatum ld, GYRedeemer la)
      }
  , ld
  )
  where
    ld :: LiquidityDatum
    ld =
      LiquidityDatum
        { ldOwner = pubKeyHashToPlutus $ piOwner pi'
        , ldNFT = tokenNameToPlutus $ piNFT pi'
        , ldFrom = assetClassToPlutus $ piFrom pi'
        , ldTo = assetClassToPlutus $ piTo pi'
        , ldPrice = ratToPair $ rationalToPlutus $ piPrice pi'
        , ldFee = ratToPair $ rationalToPlutus $ piFee pi'
        , ldGYFeeFrom = fromIntegral $ piGYFeeFrom pi'
        , ldGYFeeTo = fromIntegral $ piGYFeeTo pi'
        , ldMinFrom = piMinFrom pi'
        }

-------------------------------------------------------------------------------
-- Script address
-------------------------------------------------------------------------------

poolAddr' :: GYTxQueryMonad m => m GYAddress
poolAddr' = scriptAddress liquidityValidator

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------

listPools :: GYTxQueryMonad m => (GYAssetClass -> Bool) -> m (Map.Map GYTxOutRef PoolInfo)
listPools assetFilter = do
  addr <- poolAddr'
  utxos <-
    filterUTxOs
      ( \GYUTxO {utxoValue} ->
          any
            (\(asc, x) -> assetFilter asc && x > 0)
            $ valueToList utxoValue
      )
      <$> utxosAtAddress' addr
  datums <- utxosDatums utxos
  iwither (\oref vod -> either (const Nothing) Just <$> runExceptT (makePoolInfo oref vod)) datums

getPoolInfo :: GYTxQueryMonad m => GYTxOutRef -> m PoolInfo
getPoolInfo poolRef = do
  res <- runExceptT $ do
    utxo <-
      maybeToExceptT (printf "unknown pool reference %s" $ show poolRef) $
        utxoAtTxOutRef' poolRef

    vod <-
      maybeToExceptT (printf "invalid pool reference %s (wrong or unknown datum)" $ show poolRef) $
        utxoDatum utxo

    makePoolInfo poolRef vod

  either (throwError . GYTxMonadException) return res

makePoolInfo :: GYTxQueryMonad m => GYTxOutRef -> (GYAddress, GYValue, LiquidityDatum) -> ExceptT String m PoolInfo
makePoolInfo ref (_, v, ld)
  | from' /= to'
    , gyFeeFrom >= 0
    , gyFeeTo >= 0
    , amtFrom >= 0
    , amtTo >= 0 =
    do
      owner' <- either throwError return $ pubKeyHashFromPlutus $ ldOwner ld
      nft' <- maybe (throwError "invalid nft token name") return $ tokenNameFromPlutus $ ldNFT ld
      fromAC <- maybe (throwError "invalid assetclass") return $ assetClassFromPlutus $ ldFrom ld
      toAC <- maybe (throwError "invalid assetclass") return $ assetClassFromPlutus $ ldTo ld

      price <- maybe (throwError "invalid rational provided for price") return mprice
      fee <- maybe (throwError "invalid rational provided for fee") return mfee

      unless (price > 0) (throwError "price must be > 0")
      unless (fee >= 0) (throwError "fee must be >= 0")

      return
        PoolInfo
          { piRef = ref
          , piOwner = owner'
          , piNFT = nft'
          , piFrom = fromAC
          , piTo = toAC
          , piAmtFrom = fromIntegral amtFrom
          , piAmtTo = fromIntegral amtTo
          , piGYFeeFrom = fromIntegral gyFeeFrom
          , piGYFeeTo = fromIntegral gyFeeTo
          , piPrice = rationalFromGHC price
          , piFee = rationalFromGHC fee
          , piMinFrom = ldMinFrom ld
          }
  | otherwise = throwError "invalid pool info datum"
  where
    from', to' :: Plutus.AssetClass
    from' = ldFrom ld
    to' = ldTo ld

    mprice, mfee :: Maybe Rational
    mprice = toGHC <$> pairToRat (ldPrice ld)
    mfee = toGHC <$> pairToRat (ldFee ld)

    gyFeeFrom, gyFeeTo :: Integer
    gyFeeFrom = ldGYFeeFrom ld
    gyFeeTo = ldGYFeeTo ld

    amtFrom, amtTo :: Integer
    amtFrom = Plutus.assetClassValueOf (valueToPlutus v) from' - gyFeeFrom
    amtTo = Plutus.assetClassValueOf (valueToPlutus v) to' - gyFeeTo

maybeToExceptT :: Functor m => e -> m (Maybe a) -> ExceptT e m a
maybeToExceptT err m = ExceptT $ fmap (maybe (Left err) Right) m

-------------------------------------------------------------------------------
-- Tx constructors
-------------------------------------------------------------------------------

openPool ::
  (HasCallStack, GYTxMonad m) =>
  (Natural, GYAssetClass) ->
  (Natural, GYAssetClass) ->
  Rational ->
  Rational ->
  Integer ->
  m GYTxSkeleton
openPool (fromAmt', fromAC) (toAmt', toAC) price fee minFrom = do
  utxo <- someUTxO
  pkh <- ownPubKeyHash
  outAddr <- poolAddr'

  tn' <- case expectedTokenName utxo of
    Nothing -> throwError $ fromString $ printf "invalid token name for ref %s" utxo
    Just tn' -> return tn'
  let tn = tokenNameToPlutus tn'
  let ld =
        LiquidityDatum
          { ldOwner = pubKeyHashToPlutus pkh
          , ldNFT = tn
          , ldFrom = assetClassToPlutus fromAC
          , ldTo = assetClassToPlutus toAC
          , ldPrice = ratToPair $ fromGHC price
          , ldFee = ratToPair $ fromGHC fee
          , ldGYFeeFrom = 0
          , ldGYFeeTo = 0
          , ldMinFrom = minFrom
          }
      nftValue = valueSingleton (GYToken nftMintingPolicyId tn') 1
      fromAmt = fromIntegral fromAmt'
      toAmt = fromIntegral toAmt'
      value = nftValue <> valueSingleton fromAC fromAmt <> valueSingleton toAC toAmt
      o =
        GYTxOut
          { gyTxOutAddress = outAddr
          , gyTxOutValue = value
          , gyTxOutDatum = Just $ GYDatum ld
          }

  return $
    mustHaveInput (GYTxIn utxo Nothing)
      <> mustHaveOutput o
      <> mustMint nftMintingPolicy (GYRedeemer $ Just $ txOutRefToPlutus utxo) tn' 1

updatePool ::
  GYTxMonad m =>
  GYTxOutRef ->
  GYPubKeyHash ->
  Natural ->
  Natural ->
  Rational ->
  Rational ->
  Integer ->
  m GYTxSkeleton
updatePool poolRef owner fromAmt' toAmt' price fee minFrom = do
  addr <- poolAddr'
  pkh <- ownPubKeyHash
  pi' <- getPoolInfo poolRef
  let (i, ld) = poolInfoToIn pi' Update
      ld' =
        ld
          { ldOwner = pubKeyHashToPlutus owner
          , ldPrice = ratToPair $ fromGHC price
          , ldFee = ratToPair $ fromGHC fee
          , ldMinFrom = minFrom
          }

      value :: GYValue
      value =
        valueSingleton (GYToken nftMintingPolicyId (piNFT pi')) 1
          <> valueSingleton (piFrom pi') (fromIntegral fromAmt')
          <> valueSingleton (piTo pi') (fromIntegral toAmt')

      o =
        GYTxOut
          { gyTxOutAddress = addr
          , gyTxOutValue = value
          , gyTxOutDatum = Just $ GYDatum ld'
          }

  return $
    mconcat
      [ mustHaveInput i
      , mustHaveOutput o
      , mustBeSignedBy pkh
      ]

data SwapDirection = FromTo | ToFrom
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

swapPool ::
  GYTxMonad m =>
  GYTxOutRef ->
  Natural ->
  SwapDirection ->
  m GYTxSkeleton
swapPool poolRef diff dir = do
  addr <- poolAddr'
  pi' <- getPoolInfo poolRef
  let (i, o) = swap' addr pi' diff dir
  return $
    mconcat
      [ mustHaveInput i
      , mustHaveOutput o
      ]

swap' ::
  -- | pool address
  GYAddress ->
  PoolInfo ->
  Natural ->
  SwapDirection ->
  (GYTxIn, GYTxOut)
swap' addr pi' diff FromTo =
  let (txIn, ld) = poolInfoToIn pi' Swap
      (gyFee', amtTo', _) = calculateSwap FromTo pi' diff
      ld' = ld {ldGYFeeFrom = fromIntegral gyFee' + ldGYFeeFrom ld}

      value :: GYValue
      value =
        valueSingleton (GYToken nftMintingPolicyId (piNFT pi')) 1
          <> valueSingleton (piFrom pi') (fromIntegral $ diff + piAmtFrom pi' + piGYFeeFrom pi')
          <> valueSingleton (piTo pi') (fromIntegral $ amtTo' + piGYFeeTo pi')

      o =
        GYTxOut
          { gyTxOutAddress = addr
          , gyTxOutValue = value
          , gyTxOutDatum = Just $ GYDatum ld'
          }
   in (txIn, o)
swap' addr pi' diff ToFrom =
  let (txIn, ld) = poolInfoToIn pi' Swap
      (gyFee', amtFrom', _) = calculateSwap ToFrom pi' diff
      ld' = ld {ldGYFeeTo = fromIntegral gyFee' + ldGYFeeTo ld}
      value :: GYValue
      value =
        valueSingleton (GYToken nftMintingPolicyId (piNFT pi')) 1
          <> valueSingleton (piTo pi') (fromIntegral $ diff + piAmtTo pi' + piGYFeeTo pi')
          <> valueSingleton (piFrom pi') (fromIntegral $ amtFrom' + piGYFeeFrom pi')
      o =
        GYTxOut
          { gyTxOutAddress = addr
          , gyTxOutValue = value
          , gyTxOutDatum = Just $ GYDatum ld'
          }
   in (txIn, o)

calculateSwap :: SwapDirection -> PoolInfo -> Natural -> (Natural, Natural, Natural)
calculateSwap sd pi' diffIn =
  let fee = piFee pi'
      gyFee = gyFee
      (price, oldOut) = case sd of
        FromTo -> (piPrice pi', piAmtTo pi')
        ToFrom -> (recip $ piPrice pi', piAmtFrom pi')
      fee' = min diffIn $ ceiling $ fromIntegral diffIn * fee
      gyFee' = min fee' $ ceiling $ toRational fee' * gyFee
      diffOut = truncate $ fromIntegral (diffIn - fee') * price
      newOut
        | diffOut <= oldOut = oldOut - diffOut
        | otherwise = 0
   in (gyFee', newOut, oldOut - newOut)

collectPool ::
  GYTxMonad m =>
  GYTxOutRef ->
  m GYTxSkeleton
collectPool poolRef = do
  pkh <- ownPubKeyHash
  pi' <- getPoolInfo poolRef
  paddr <- poolAddr'

  let (i, ld) = poolInfoToIn pi' CollectFees
      ld' = ld {ldGYFeeFrom = 0, ldGYFeeTo = 0}
      value :: GYValue
      value =
        valueSingleton (GYToken nftMintingPolicyId (piNFT pi')) 1
          <> valueSingleton (piFrom pi') (fromIntegral $ piAmtFrom pi')
          <> valueSingleton (piTo pi') (fromIntegral $ piAmtTo pi')
      o =
        GYTxOut
          { gyTxOutAddress = paddr
          , gyTxOutValue = value
          , gyTxOutDatum = Just $ GYDatum ld'
          }

  return $
    mconcat
      [ mustHaveInput i
      , mustHaveOutput o
      , mustBeSignedBy pkh
      ]

closePool ::
  GYTxMonad m =>
  GYTxOutRef ->
  m GYTxSkeleton
closePool poolRef = do
  pkh <- ownPubKeyHash
  pi' <- getPoolInfo poolRef

  let (i, _) = poolInfoToIn pi' Close
      gyFeeFrom = piGYFeeFrom pi'
      gyFeeTo = piGYFeeTo pi'
      gyValue =
        valueSingleton (piFrom pi') (fromIntegral gyFeeFrom)
          <> valueSingleton (piTo pi') (fromIntegral gyFeeTo)

      -- fee output
      feeOutput
        | gyFeeFrom > 0
            || gyFeeTo > 0 =
          Just
            GYTxOut
              { gyTxOutAddress = gyAddress
              , gyTxOutValue = gyValue
              , gyTxOutDatum = Just $ GYDatum $ txOutRefToPlutus poolRef
              }
        | otherwise = Nothing

      tn = piNFT pi'

  return $
    mustHaveInput i
      <> mustHaveOptionalOutput feeOutput
      <> mustMint nftMintingPolicy (GYRedeemer (Nothing :: Maybe Plutus.TxOutRef)) tn (-1)
      <> mustBeSignedBy pkh

ratToPair :: PlutusTx.Rational -> (PlutusTx.Integer, PlutusTx.Integer)
ratToPair r = (PlutusTx.numerator r, PlutusTx.denominator r)

pairToRat :: (PlutusTx.Integer, PlutusTx.Integer) -> Maybe PlutusTx.Rational
pairToRat (n, d) = PlutusTx.ratio n d
