{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import qualified Data.Aeson                           as Aeson
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GeniusYield.OrderBot.Config
import           GHC.Generics                         (Generic)
import           GHC.IO                               (throwIO)
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Plutus
import qualified Plutus.V1.Ledger.Api                 as Plutus
import qualified PlutusTx.Prelude                     as Plutus
import qualified PlutusTx                             as Plutus
import           System.Environment                   (getArgs)
import           Text.Printf                          (printf)

main :: IO ()
main = getCfg >>= mintTokens >>= print

getCfg :: IO Config
getCfg = do
    xs <- getArgs
    case xs of
        [file] -> do
            e <- Aeson.eitherDecodeFileStrict' file
            case e of
                Left err  -> throwIO $ userError err
                Right cfg -> return cfg
        _      -> throwIO $ userError "expected exactly one argument, the path to the config file"

data Config = Config
    { cfgInfo           :: !GYCoreProviderInfo
    , cfgNetworkId      :: !GYNetworkId
    , cfgSigningKeyFile :: !FilePath
    , cfgAmount         :: !Natural
    , cfgTokenName      :: !GYTokenName
    } deriving (Show, Generic, Aeson.FromJSON)

{-# INLINABLE mkTokenPolicy' #-}
mkTokenPolicy' :: Plutus.PubKeyHash -> () -> Plutus.ScriptContext -> Bool
mkTokenPolicy' pkh _ ctx = pkh `Plutus.elem` Plutus.txInfoSignatories (Plutus.scriptContextTxInfo ctx)

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: Plutus.PubKeyHash -> Plutus.BuiltinData -> Plutus.BuiltinData -> ()
mkTokenPolicy pkh = Plutus.mkUntypedMintingPolicy Plutus.$ mkTokenPolicy' pkh

tokenPolicy :: GYPubKeyHash -> GYMintingPolicy
tokenPolicy pkh =
    mintingPolicyFromPlutus $ Plutus.mkMintingPolicyScript $
        $$(Plutus.compile [|| mkTokenPolicy ||])
            `Plutus.applyCode` Plutus.liftCode (pubKeyHashToPlutus pkh)

mintTokens :: Config -> IO GYTxId
mintTokens Config {..} = do
    skey <- readPaymentSigningKey cfgSigningKeyFile
    let pkh    = pubKeyHash $ paymentVerificationKey skey
        addr   = addressFromPubKeyHash cfgNetworkId pkh
        policy = tokenPolicy pkh

    printf "%s\n" $ GYToken (mintingPolicyId policy) cfgTokenName

    (collateral, _) <- withCfgProviders (GYCoreConfig cfgInfo cfgNetworkId) $ \providers ->
        runGYTxQueryMonadNode cfgNetworkId providers $ getCollateral addr 5_000_000
    withCfgProviders (GYCoreConfig cfgInfo cfgNetworkId) $ \providers -> do
        txBody <- runGYTxMonadNode cfgNetworkId providers addr collateral $
            return $
                mustMint policy (GYRedeemer ()) cfgTokenName (toInteger cfgAmount) <>
                mustBeSignedBy pkh
        gySubmitTx providers $ signTx txBody skey
