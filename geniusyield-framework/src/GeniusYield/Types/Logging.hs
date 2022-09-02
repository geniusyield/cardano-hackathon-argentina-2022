module GeniusYield.Types.Logging
    ( -- * Severity
      GYLogSeverity (..)
      -- * Verbosity
    , GYLogVerbosity
      -- * Namespace
    , GYLogNamespace
      -- * Scribe Configuration
    , GYLogScribeType (..)
    , GYLogScribeConfig (..)
      -- * Utilities
    , logSeverityToKatip
    , logVerbosityToKatip
    , logNamespaceToKatip
    , prettyNamespace
    , mkLogEnv
    ) where

import           Data.Aeson          (Key)
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Key      as Key
import           Data.List           (intercalate)
import qualified Data.Text           as Text
import qualified Katip               as K
import           System.IO           (stderr)
import qualified Text.Printf         as Printf

import           GeniusYield.Imports

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import           Text.Printf                (printf)

-------------------------------------------------------------------------------
-- Severity
-------------------------------------------------------------------------------

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode GYDebug
-- "Debug"
--
-- >>> LBS8.putStrLn $ Aeson.encode GYInfo
-- "Info"
--
-- >>> LBS8.putStrLn $ Aeson.encode GYWarning
-- "Warning"
--
-- >>> LBS8.putStrLn $ Aeson.encode GYError
-- "Error"
--
data GYLogSeverity = GYDebug | GYInfo | GYWarning | GYError
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

instance Aeson.ToJSON GYLogSeverity where
    toJSON GYDebug   = "Debug"
    toJSON GYInfo    = "Info"
    toJSON GYWarning = "Warning"
    toJSON GYError   = "Error"

-- |
--
-- >>> Aeson.eitherDecode @GYLogSeverity "\"Debug\""
-- Right GYDebug
--
-- >>> Aeson.eitherDecode @GYLogSeverity "\"Info\""
-- Right GYInfo
--
-- >>> Aeson.eitherDecode @GYLogSeverity "\"Warning\""
-- Right GYWarning
--
-- >>> Aeson.eitherDecode @GYLogSeverity "\"Error\""
-- Right GYError
--
-- >>> Aeson.eitherDecode @GYLogSeverity "\"Fatal\""
-- Left "Error in $: unknown GYLogSeverity: Fatal"
--
instance Aeson.FromJSON GYLogSeverity where
    parseJSON = Aeson.withText "GYLogSeverity" $ \t ->
        if | t == "Debug"   -> return GYDebug
           | t == "Info"    -> return GYInfo
           | t == "Warning" -> return GYWarning
           | t == "Error"   -> return GYError
           | otherwise      -> fail $ "unknown GYLogSeverity: " <> Text.unpack t

logSeverityToKatip :: GYLogSeverity -> K.Severity
logSeverityToKatip GYDebug   = K.DebugS
logSeverityToKatip GYInfo    = K.InfoS
logSeverityToKatip GYWarning = K.WarningS
logSeverityToKatip GYError   = K.ErrorS

-------------------------------------------------------------------------------
-- Verbosity
-------------------------------------------------------------------------------

-- |
--
-- >>> Aeson.eitherDecode @GYLogVerbosity "\"V0\""
-- Right (GYLogVerbosity V0)
--
-- >>> Aeson.eitherDecode @GYLogVerbosity "\"V1\""
-- Right (GYLogVerbosity V1)
--
-- >>> Aeson.eitherDecode @GYLogVerbosity "\"V2\""
-- Right (GYLogVerbosity V2)
--
-- >>> Aeson.eitherDecode @GYLogVerbosity "\"V3\""
-- Right (GYLogVerbosity V3)
--
-- >>> Aeson.eitherDecode @GYLogVerbosity "\"V4\""
-- Left "Error in $: Invalid Verbosity V4"
--
newtype GYLogVerbosity = GYLogVerbosity K.Verbosity
    deriving stock   (Show, Read)
    deriving newtype (Eq, Ord, Enum, Bounded, Aeson.FromJSON, Aeson.ToJSON)

logVerbosityToKatip :: GYLogVerbosity -> K.Verbosity
logVerbosityToKatip = coerce

-------------------------------------------------------------------------------
-- Namespace
-------------------------------------------------------------------------------

-- |
--
-- >>> "My" <> "Namespace" :: GYLogNamespace
-- GYLogNamespace (Namespace {unNamespace = ["My","Namespace"]})
--
newtype GYLogNamespace = GYLogNamespace K.Namespace
    deriving stock   (Show, Read, Eq, Ord)
    deriving newtype (Semigroup, Monoid, IsString)

-- |
--
-- >>> printf "%s" ("My" <> "Namespace" :: GYLogNamespace)
-- My.Namespace
--
instance Printf.PrintfArg GYLogNamespace where
    formatArg ns ff = Printf.formatArg (prettyNamespace ns) ff

logNamespaceToKatip :: GYLogNamespace -> K.Namespace
logNamespaceToKatip = coerce

prettyNamespace :: GYLogNamespace -> String
prettyNamespace ns = intercalate "." $ map Text.unpack $ K.unNamespace $ logNamespaceToKatip ns

-------------------------------------------------------------------------------
-- Scribe Configuration
-------------------------------------------------------------------------------

data GYLogScribeType = GYStdErrScribe | GYFileScribe !FilePath
    deriving (Show, Read, Eq, Ord)

stdErrTag, fileTag, typeTag, severityTag, verbosityTag :: Key
stdErrTag    = "stderr"
fileTag      = "file"
typeTag      = "type"
severityTag  = "severity"
verbosityTag = "verbosity"

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode GYStdErrScribe
-- {"tag":"stderr"}
--
-- >>> LBS8.putStrLn $ Aeson.encode $ GYFileScribe "log.txt"
-- {"tag":"file","file":"log.txt"}
--
instance Aeson.ToJSON GYLogScribeType where
    toJSON GYStdErrScribe   = Aeson.object ["tag" Aeson..= stdErrTag]
    toJSON (GYFileScribe f) = Aeson.object
        [ "tag"  Aeson..= fileTag
        , "file" Aeson..= f]

    toEncoding GYStdErrScribe = Aeson.pairs ("tag" Aeson..= stdErrTag)
    toEncoding (GYFileScribe f) = Aeson.pairs
        ( "tag" Aeson..= fileTag <> "file" Aeson..= f )


-- |
--
-- >>> Aeson.eitherDecode @GYLogScribeType "{\"tag\":\"stderr\"}"
-- Right GYStdErrScribe
--
-- >>> Aeson.eitherDecode @GYLogScribeType "{\"tag\":\"file\",\"file\":\"log.txt\"}"
-- Right (GYFileScribe "log.txt")
--
-- >>> Aeson.eitherDecode @GYLogScribeType "{\"tag\":\"fancy-scribe\"}"
-- Left "Error in $: unknown GYLogScribe tag: fancy-scribe"
--
instance Aeson.FromJSON GYLogScribeType where
    parseJSON = Aeson.withObject "GYLogScribeType" $ \x -> do
        tag <- x Aeson..: "tag"
        if | tag == stdErrTag -> return GYStdErrScribe
           | tag == fileTag   -> GYFileScribe <$> x Aeson..: "file"
           | otherwise        -> fail $ "unknown GYLogScribe tag: " <> Key.toString tag

data GYLogScribeConfig = GYLogScribeConfig
        { cfgLogType      :: !GYLogScribeType
        , cfgLogSeverity  :: !GYLogSeverity
        , cfgLogVerbosity :: !GYLogVerbosity
        } deriving (Show, Read, Eq, Ord)

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode $ GYLogScribeConfig (GYFileScribe "log.txt") GYWarning (read "GYLogVerbosity V1")
-- {"type":{"tag":"file","file":"log.txt"},"severity":"Warning","verbosity":"V1"}
--
instance Aeson.ToJSON GYLogScribeConfig where
    toJSON GYLogScribeConfig {..} = Aeson.object
        [ typeTag      Aeson..= cfgLogType
        , severityTag  Aeson..= cfgLogSeverity
        , verbosityTag Aeson..= cfgLogVerbosity
        ]

    toEncoding GYLogScribeConfig {..} = Aeson.pairs
        ( typeTag      Aeson..= cfgLogType
        <> severityTag  Aeson..= cfgLogSeverity
        <> verbosityTag Aeson..= cfgLogVerbosity
        )


-- |
--
-- >>> Aeson.decode @GYLogScribeConfig "{\"severity\":\"Warning\",\"verbosity\":\"V1\",\"type\":{\"tag\":\"file\",\"file\":\"log.txt\"}}"
-- Just (GYLogScribeConfig {cfgLogType = GYFileScribe "log.txt", cfgLogSeverity = GYWarning, cfgLogVerbosity = GYLogVerbosity V1})
--
instance Aeson.FromJSON GYLogScribeConfig where
    parseJSON = Aeson.withObject "GYLogScribeConfig" $ \x ->
        GYLogScribeConfig <$> (x Aeson..: typeTag)
                          <*> (x Aeson..: severityTag)
                          <*> (x Aeson..: verbosityTag)

mkScribe :: GYLogScribeConfig -> IO (K.Scribe, Text.Text)
mkScribe GYLogScribeConfig {..} = case cfgLogType of
    GYStdErrScribe -> do
        scribe <-  K.mkHandleScribe K.ColorIfTerminal stderr permit verbosity
        return (scribe, "stderr")
    GYFileScribe f -> do
        scribe <- K.mkFileScribe f permit verbosity
        return (scribe, Text.pack $ "file:" <> f)
  where
    permit :: K.PermitFunc
    permit = K.permitItem $ logSeverityToKatip cfgLogSeverity

    verbosity :: K.Verbosity
    verbosity = logVerbosityToKatip cfgLogVerbosity

mkLogEnv :: GYLogNamespace -> [GYLogScribeConfig] -> IO K.LogEnv
mkLogEnv ns cfgs = do
    logEnv <- K.initLogEnv (logNamespaceToKatip $ "GeniusYield" <> ns) ""
    foldM f logEnv cfgs
  where
    f :: K.LogEnv -> GYLogScribeConfig -> IO K.LogEnv
    f logEnv cfg = do
        (scribe, name) <- mkScribe cfg
        K.registerScribe name scribe K.defaultScribeSettings logEnv
