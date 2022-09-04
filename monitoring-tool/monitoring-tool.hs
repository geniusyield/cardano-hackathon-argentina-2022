{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE RecordWildCards  #-}

import           Control.Monad               (forever, forM_, void)
import qualified Data.Aeson                  as Aeson
import           Data.Function               (on)
import           Data.List                   (sortBy)
import           Data.Text                   (Text)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GeniusYield.OrderBot.Config
import           GHC.Generics                (Generic)
import           GHC.IO                      (throwIO)
import           System.Console.ANSI
import           System.Environment          (getArgs)
import           Text.Printf                 (printf)

main :: IO ()
main = getCfg >>= monitorIO

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

data Team = Team
    { teamName  :: !Text
    , teamAddrBot :: !GYAddressBech32
    , teamAddrLP  :: !GYAddressBech32
    } deriving (Show, Generic, Aeson.FromJSON)

data Config = Config
    { cfgInfo      :: !GYCoreProviderInfo
    , cfgNetworkId :: !GYNetworkId
    , cfgTokenA    :: !GYAssetClass
    , cfgTokenB    :: !GYAssetClass
    , cfgPrice     :: !GYRational
    , cfgTeams     :: ![Team]
    } deriving (Show, Generic, Aeson.FromJSON)

data TeamInfo = TeamInfo
    { tiName   :: !Text
    , tiBotA   :: !Natural
    , tiBotB   :: !Natural
    , tiLPA    :: !Natural
    , tiLPB    :: !Natural
    , tiTotalB :: !GYRational
    } deriving (Show, Read, Eq, Ord)

runQuery :: Config -> GYTxQueryMonadNode a -> IO a
runQuery Config {..} m =
    withCfgProviders (GYCoreConfig cfgInfo cfgNetworkId) $ \providers ->
        runGYTxQueryMonadNode cfgNetworkId providers m

teamInfoIO :: Config -> Team -> IO TeamInfo
teamInfoIO cfg@Config {..} Team {..} = runQuery cfg $ do
    (botA, botB) <- f teamAddrBot
    (lpA, lpB)   <- f teamAddrLP
    let t = fromIntegral (botB + lpB) + cfgPrice * (fromIntegral $ botA + lpA)
    return TeamInfo
        { tiName   = teamName
        , tiBotA   = botA
        , tiBotB   = botB
        , tiLPA    = lpA
        , tiLPB    = lpB
        , tiTotalB = t
        }
  where
    f :: GYAddressBech32 -> GYTxQueryMonadNode (Natural, Natural)
    f addr = do
        utxos <- utxosAtAddress' $ addressFromBech32 addr
        let v  = foldMapUTxOs utxoValue utxos
            na = fromInteger $ valueAssetClass v cfgTokenA
            nb = fromInteger $ valueAssetClass v cfgTokenB
        return (na, nb)

teamInfosIO :: Config -> IO [TeamInfo]
teamInfosIO cfg@Config {..} = sortBy (compare `on` tiTotalB) <$> mapM (teamInfoIO cfg) cfgTeams

monitorIO :: Config -> IO a
monitorIO cfg = go
  where
    go = forever $ do
        infos <- teamInfosIO cfg

        clearScreen
        setCursorPosition 0 0
        setSGR [SetConsoleIntensity BoldIntensity]
        printf "Team Name                 Bot A      Bot B       LP A       LP B    Total A    Total B      Total\n\n"
        setSGR [Reset]
        forM_ infos $ \TeamInfo {..} ->
            printf "%-20s %10d %10d %10d %10d %10d %10d %s%10d%s\n"
                tiName
                tiBotA
                tiBotB
                tiLPA
                tiLPB
                (tiBotA + tiLPA)
                (tiBotB + tiLPB)
                (setSGRCode [SetConsoleIntensity BoldIntensity])
                (round tiTotalB :: Natural)
                (setSGRCode [])

        waitForNextBlock cfg

waitForNextBlock :: Config -> IO ()
waitForNextBlock cfg = void $ gyWaitForNextBlockDefault $ runQuery cfg currentSlot'
