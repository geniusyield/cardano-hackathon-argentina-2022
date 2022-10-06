{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wwarn                #-}

module Main (main, mint4All) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import Data.Foldable
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Set as Set
import Data.Traversable
import GHC.Float (double2Int)
import GHC.Generics (Generic)
import GHC.Natural (intToNatural)
import GeniusYield.DEX.Api.PartialOrder
import GeniusYield.OrderBot.Config
import GeniusYield.Simulator.Generator (genPercentage, randTokenAUPrice, randTokenQ)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Scripts
import qualified PlutusCore as PLC
import System.Environment
import System.Exit
import System.FilePath
import UntypedPlutusCore

-----------------------------------------------------------------------
----------------------------- HOW_TO_USE -------------------------------
{-
This script is meant to continuously place orders between two tokens
with the prices following a mirrored pareto distribution.

To use this, you'll need a directory containing a file where the user's skeys and collaterals
are defined. This can be passed from the commandline.
An example is in: geniusyield-simulator/army/users.json

The containing directory should also contain a directory named "users" that has all the user
signing keys.

For the provided setup, an example commandline "armyPath" would be "geniusyield-simulator/army" from project
root.
-}

data ConfigPaths = ConfigPaths
  { -- | Path to a directory with `users.json` and a `users` directory with all the keys.
    armyPath :: !FilePath
  , -- | Path to the primary signing keys directory.
    mainSignersPath :: !FilePath
  , -- | Path to GY framework config.
    frameworkCfgPath :: !FilePath
  }
  deriving stock (Eq, Show)

{-
1. Run the `mint4All x` function. It'll give `x` of each token to
each user. (E.g. `> mint4All 10000`)
2. 
  A. Run `main` to start generating orders.
  B. Run using commands in Makefile.
-}

-----------------------------------------------------------------------
------------------------------- SETUP ---------------------------------

data User = User
  { userId :: !String
  , sKey :: GYPaymentSigningKey
  , coll :: GYTxOutRef
  }
  deriving stock (Generic, Show)

data UserRaw = UserRaw
  { sKeyPath :: FilePath
  , collRaw :: GYTxOutRef
  }
  deriving stock (Generic, Show)

instance Aeson.FromJSON User

instance Aeson.ToJSON User

instance Aeson.FromJSON UserRaw

instance Aeson.ToJSON UserRaw

dummyATkname :: GYTokenName
dummyATkname = "TokenA"

dummyBTkname :: GYTokenName
dummyBTkname = "TokenB"

getConfig :: ConfigPaths -> IO (GYNetworkId, GYProviders)
getConfig ConfigPaths {frameworkCfgPath} = do
  e <- Aeson.eitherDecodeFileStrict' frameworkCfgPath
  case e of
    Left err -> throwIO $ userError $ "error reading config-file: " <> err
    Right cfg@GYCoreConfig {cfgNetworkId} -> do
      putStrLn $ "read configuration: " <> show cfg
      withCfgProviders cfg $ pure . (cfgNetworkId,)

getUsers :: ConfigPaths -> IO [User]
getUsers ConfigPaths {armyPath} = do
  e <- Aeson.eitherDecodeFileStrict' $ armyPath </> "users.json"
  case e of
    Left err -> throwIO $ userError $ "error reading users-file: " <> err
    Right usersRaw -> do
      putStrLn $ "users: " <> show usersRaw
      for usersRaw $ \UserRaw {sKeyPath, collRaw} -> do
        sKey <- readPaymentSigningKey $ armyPath </> "users" </> sKeyPath
        pure $ User {userId = sKeyPath, sKey = sKey, coll = collRaw}

-- | user01 sends tAda to another user.
send500TAda :: ConfigPaths -> GYNetworkId -> GYProviders -> Int -> IO ()
send500TAda ConfigPaths {armyPath, mainSignersPath} netId providers user = do
  -- The path to new user signing key
  skeyS <- readPaymentSigningKey $ mainSignersPath </> "user01.skey"
  skeyR <- readPaymentSigningKey $ armyPath </> "users" </> ("user" ++ show user ++ ".skey")
  let addrS = addrFromSkey netId skeyS
      addrR = addrFromSkey netId skeyR
      -- dummy utxo ref, doesn't matter.
      collateral = "b0b27bf4e5415f7509eddefdb38e833750204d20c35edfd2c28331be3e62fcc5#1"
  -- create collateral
  txBody <-
    runGYTxMonadNode netId providers addrS collateral . pure $
      mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addrR
          , gyTxOutDatum = Nothing
          , gyTxOutValue = valueFromLovelace 500_000_000 -- 500 ada
          }
  let tx = signTx txBody skeyS
  tid <- gySubmitTx providers tx
  print $ "Gave 500 Ada to user " ++ show user ++ " in Tx " ++ show tid

-- | user01 sends tAda and tokens A and B to another user.
sendAdaAndTokens :: ConfigPaths -> GYNetworkId -> GYProviders -> Int -> IO ()
sendAdaAndTokens ConfigPaths {armyPath, mainSignersPath} netId providers user = do
  -- The path to new user signing key
  skeyS <- readPaymentSigningKey $ mainSignersPath </> "user01.skey"
  skeyR <- readPaymentSigningKey $ armyPath </> "users" </> ("user" ++ show user ++ ".skey")
  let addrS = addrFromSkey netId skeyS
      addrR = addrFromSkey netId skeyR
      -- dummy utxo ref, doesn't matter.
      collateral = "b0b27bf4e5415f7509eddefdb38e833750204d20c35edfd2c28331be3e62fcc5#1"
  -- create collateral
  txBody <-
    runGYTxMonadNode netId providers addrS collateral . pure $
      mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addrR
          , gyTxOutDatum = Nothing
          , gyTxOutValue =
              valueFromLovelace 500_000_000
                <> valueSingleton tokenA 200_000 -- 500 ada
                <> valueSingleton tokenB 200_000
          }
  let tx = signTx txBody skeyS
  tid <- gySubmitTx providers tx
  print $ "Gave 500 Ada, 200,000 A and 200,000 B to user " ++ show user ++ " in Tx " ++ show tid

tokenA, tokenB :: GYAssetClass
tokenA = "fa2261efef35419afb055426b651b404356d803901a89c14cee9a926.TokenA"
tokenB = "fa2261efef35419afb055426b651b404356d803901a89c14cee9a926.TokenB"

-- | user01 sends tAda to army. Have to wait between Tx
send500ToMany :: ConfigPaths -> [Int] -> IO ()
send500ToMany cfg x = do
  (netId, providers) <- getConfig cfg
  mapM_ (\u -> send500TAda cfg netId providers u >> threadDelay 90_000_000) x

paths :: ConfigPaths
paths =
  ConfigPaths
    { armyPath = "army"
    , mainSignersPath = "../signing-keys"
    , frameworkCfgPath = "../config-local.json"
    }

send500ToMany' :: IO ()
send500ToMany' = send500ToMany paths [3]

-- | Each user creates its own collateral
createCollateral :: ConfigPaths -> Int -> IO ()
createCollateral cfg user = do
  (netId, providers) <- getConfig cfg
  -- The path to new user signing key
  skey <- readPaymentSigningKey $ "army/users/user" ++ show user ++ ".skey"
  let addr = addrFromSkey netId skey
      -- dummy utxo ref, doesn't matter.
      collateral = "b0b27bf4e5415f7509eddefdb38e833750204d20c35edfd2c28331be3e62fcc5#1"
  -- create collateral
  txBody <-
    runGYTxMonadNode netId providers addr collateral . pure $
      mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addr
          , gyTxOutDatum = Nothing
          , gyTxOutValue = valueFromLovelace 5_000_000 -- 5 ada collateral
          }
  let tx = signTx txBody skey
  tid <- gySubmitTx providers tx
  print $ "User " ++ show user ++ " collateral: " ++ show tid ++ "#1"

createCollateral' :: Int -> IO ()
createCollateral' = createCollateral paths

-----------------------------------------------------------------------
----------------------------- MINTING ---------------------------------

-- | "Always succeeds" 'MintingPolicy' UPLC logic; for testing use.
alwaysSucceedsUPLC :: Program DeBruijn DefaultUni DefaultFun ()
alwaysSucceedsUPLC =
  Program () (Version () 1 0 0) $
    LamAbs
      ()
      (DeBruijn 0)
      ( LamAbs
          ()
          (DeBruijn 0)
          (Constant () (PLC.Some (PLC.ValueOf PLC.DefaultUniUnit ())))
      )

-- | Always succeeds 'MintingPolicy'; for testing use.
dummyTkMp :: GYMintingPolicy
dummyTkMp = mintingPolicyFromPlutus . MintingPolicy $ Script alwaysSucceedsUPLC

dummyMint :: GYTokenName -> Integer -> GYTxSkeleton
dummyMint = mustMint dummyTkMp (GYRedeemer ())

-- | Mint "amount" of dummyA and dummyB tokens for "user".
mintTokens :: (GYNetworkId, GYProviders) -> User -> Integer -> IO ()
mintTokens (netId, providers) User {sKey, coll} amount = do
  let addr = addrFromSkey netId sKey
  txBody <-
    runGYTxMonadNode netId providers addr coll $
      pure $
        dummyMint dummyATkname amount
          <> dummyMint dummyBTkname amount
  let tx = signTx txBody sKey
  tid <- gySubmitTx providers tx
  putStrLn $
    "Minted " <> show amount <> " " <> show dummyATkname <> " and "
      <> show dummyBTkname
      <> " for address "
      <> show addr
      <> " in Tx "
      <> show tid

-- | Give "amount" of each token to each user.
mint4All :: ConfigPaths -> Integer -> IO ()
mint4All cfg amount = do
  users <- getUsers cfg
  frameworkCfg <- getConfig cfg
  mapM_ (\u -> mintTokens frameworkCfg u amount) users
  print @String "Done!"

mint4All' :: Integer -> IO ()
mint4All' = mint4All paths

-----------------------------------------------------------------------
------------------------- PLACING_ORDERS ------------------------------

dummyAssetCls :: GYTokenName -> GYAssetClass
dummyAssetCls = GYToken (mintingPolicyId dummyTkMp)

getValues :: IO (Double, Int, Double)
getValues = do
  priceTokenA <- randTokenAUPrice
  tokenQ <- randTokenQ
  perc <- genPercentage
  return (priceTokenA, tokenQ, perc)

pkhFromSkey :: GYPaymentSigningKey -> GYPubKeyHash
pkhFromSkey = pubKeyHash . paymentVerificationKey

addrFromSkey :: GYNetworkId -> GYPaymentSigningKey -> GYAddress
addrFromSkey netId = addressFromPubKeyHash netId . pkhFromSkey

-- | Place an order in a single Tx; continuous txs have their prices following a mirrored pareto distribution.
placeOrder :: GYNetworkId -> GYProviders -> User -> GYTokenName -> GYTokenName -> IO ()
placeOrder netId providers User {userId, sKey, coll} offTn priceTn = do
  let addr = addrFromSkey netId sKey
  (priceTokenA, tokenQ, perc) <- getValues
  let order =
        placePartialOrder
          addr
          (intToNatural tokenQ, dummyAssetCls offTn)
          (dummyAssetCls priceTn)
          (unitPrice priceTokenA)
          $ toNat (fromIntegral tokenQ * perc)
  txBody <- runGYTxMonadNode netId providers addr coll order
  let tx = signTx txBody sKey
  tid <- gySubmitTx providers tx
  print $ userId ++ " placed an order selling token " ++ show offTn ++ " in Tx " ++ show tid
  where
    toNat x = intToNatural $ double2Int x
    invRat r = denominator r % numerator r
    unitPrice pricTokA =
      if offTn == dummyATkname
        then rationalFromGHC $ toRational pricTokA
        else rationalFromGHC $ invRat $ toRational pricTokA

-- | Scan the chain for existing orders and cancel all of them in batches of 10.
cancelAllOrders :: ConfigPaths -> IO ()
cancelAllOrders cfg = do
  let go :: [User] -> GYNetworkId -> GYProviders -> [PartialOrderInfo] -> IO ()
      go users@(User {userId = user01Id, sKey = user01SKey, coll = user01Coll} : _) netId providers partialOrderInfos = do
        mapM_ (\u -> placeOrder netId providers u dummyATkname dummyBTkname) (fst $ splitMiddle users)
        mapM_ (\u -> placeOrder netId providers u dummyBTkname dummyATkname) (snd $ splitMiddle users)
        when (null partialOrderInfos) $ do
          putStrLn "---------- No more orders to cancel! -----------"
          exitSuccess
        let (batch, rest) = splitAt 10 partialOrderInfos
        txBody <-
          runGYTxMonadNode netId providers (addrFromSkey netId user01SKey) user01Coll $
            fold <$> traverse (cancelPartialOrder . poiRef) batch
        let requiredSigners = Set.fromList $ map poiOwnerKey batch
            signedTx =
              multiSignTx txBody . map sKey $
                filter
                  (\User {userId, sKey} -> user01Id == userId || pkhFromSkey sKey `Set.member` requiredSigners)
                  users
        tid <- gySubmitTx providers signedTx
        putStrLn $ "Submitted a cancel order batch: " ++ show tid
        putStrLn "---------- Done for the block! -----------"
        threadDelay 30_000_000 -- Wait 30s
        go users netId providers rest
      go [] _ _ _ = putStrLn "No users!" >> pure ()
  getUsers cfg >>= \u ->
    getConfig cfg >>= \(netId, providers) -> do
      user01Skey <- readPaymentSigningKey $ mainSignersPath cfg </> "user01.skey"
      user02Skey <- readPaymentSigningKey $ mainSignersPath cfg </> "user02.skey"
      let user01Collateral = "b0b27bf4e5415f7509eddefdb38e833750204d20c35edfd2c28331be3e62fcc5#1"
          user02Collateral = "2b44bc511b2f16758e06533faf7a106eeda0e5ec2a07af9067421e01a35711b1#1"
      partialOrderInfos <- runGYTxQueryMonadNode netId providers $ partialOrders (const True)
      go
        ( User {userId = "signers/user01.skey", sKey = user01Skey, coll = user01Collateral} :
          User {userId = "signers/user02.skey", sKey = user02Skey, coll = user02Collateral} :
          u
        )
        netId
        providers
        partialOrderInfos
  where
    splitMiddle u = splitAt (length u `div` 2) u

queryAllOrders :: ConfigPaths -> IO [PartialOrderInfo]
queryAllOrders cfg = do
  (netId, providers) <- getConfig cfg
  runGYTxQueryMonadNode netId providers . partialOrders $ const True

getOrderByRef :: ConfigPaths -> GYTxOutRef -> IO PartialOrderInfo
getOrderByRef cfg ref = do
  (netId, providers) <- getConfig cfg
  runGYTxQueryMonadNode netId providers $ getPartialOrderInfo ref

-- | Pass this either 'dummyATkname' or 'dummyATkname' alongside how many you want to burn.
botBurnDummyToken :: ConfigPaths -> GYTokenName -> Integer -> IO ()
botBurnDummyToken cfg@ConfigPaths {mainSignersPath} tkName amountToBurn = do
  (netId, providers) <- getConfig cfg
  botSkey <- readPaymentSigningKey $ mainSignersPath </> "bot.skey"
  let botAddr = addrFromSkey netId botSkey
      botCollateral = "ca54517fd33bb9abafc1b48eb7e3ee6a7681b5ef38a4eaa8e56ab717b7705622#1"
  txBody <- runGYTxMonadNode netId providers botAddr botCollateral . pure $ dummyMint tkName amountToBurn
  let tx = signTx txBody botSkey
  tid <- gySubmitTx providers tx
  print tid

-----------------------------------------------------------------------
----------------------------- MAIN ------------------------------------

main :: IO ()
main = do
  args <- getArgs
  cfg <- case args of
    [frameworkCfgPath, armyPath, mainSignersPath] ->
      pure $
        ConfigPaths
          { armyPath = armyPath
          , mainSignersPath = mainSignersPath
          , frameworkCfgPath = frameworkCfgPath
          }
    _ ->
      throwIO . userError $
        "expected exactly three command line arguments, in order: \n"
          ++ "  1. Path to the GY framework config-file\n"
          ++ "  2. Path to the user army directory, containing the users.json and users directory containing keys"
          ++ "  3. Path to the main signers directory, containing user01.skey"
  let go :: [User] -> GYNetworkId -> GYProviders -> IO ()
      go users netId providers = do
        mapM_ (\u -> placeOrder netId providers u dummyATkname dummyBTkname) (fst $ splitMiddle users)
        mapM_ (\u -> placeOrder netId providers u dummyBTkname dummyATkname) (snd $ splitMiddle users)
        putStrLn "---------- Done for the block! -----------"
        threadDelay 100_000_000 -- Wait 100s
        go users netId providers
  getUsers cfg >>= \u -> getConfig cfg >>= uncurry (go u)
  where
    splitMiddle u = splitAt (length u `div` 2) u
