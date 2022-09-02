module GeniusYield.Providers.Katip
    ( mkKatipLog
    ) where

import qualified Katip               as K

import           GeniusYield.Imports
import           GeniusYield.Types

mkKatipLog :: GYLogNamespace -> [GYLogScribeConfig] -> IO GYLog
mkKatipLog ns cfgs = do
    logEnv <- mkLogEnv ns cfgs
    let log' ns' s msg =
            K.runKatipT logEnv $ K.logLoc () (logNamespaceToKatip ns') (logSeverityToKatip s) $ K.logStr msg
    return GYLog
        { logRun     = log'
        , logCleanUp = void $ K.closeScribes logEnv
        }
