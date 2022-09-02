module GeniusYield.Types.Time
    ( FormatTime (..), ParseTime (..)
    , gyIso8601Show, gyIso8601ParseM
    , GYTime
    , getCurrentGYTime
    , timeFromPlutus
    , timeToPlutus
    , timeToPOSIX
    , timeFromPOSIX
    ) where

import           GeniusYield.Imports

import           Control.Lens                      ((&), (?~))
import qualified Data.Aeson                     as Aeson
import qualified Data.Text                      as Text
import qualified Data.Time.Clock                as Time
import qualified Data.Time.Clock.POSIX          as Time
import           Data.Time.Format.Internal         (FormatTime (..), ParseTime (..))
import qualified Data.Time.Format.ISO8601       as Time
import qualified Ledger                         as Plutus
import qualified Web.HttpApiData                as Web
import qualified Data.Swagger                   as Swagger
import qualified Data.Swagger.Internal.Schema   as Swagger

-------------------------------------------------------------------------------
-- GYTime
-------------------------------------------------------------------------------

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import           Text.Printf                (printf)
-- >>> import qualified Web.HttpApiData            as Web

newtype GYTime = GYTime Time.POSIXTime
  deriving         (Show, Read)
  deriving newtype (Eq, Ord, FormatTime)

-- |
--
-- >>> "1970-01-01T00:00:00Z" :: GYTime
-- GYTime 0s
--
-- >>> "1970-01-01T00:00:00" :: GYTime
-- *** Exception: can't parse '1970-01-01T00:00:00' as GYTime in ISO8601 format
-- ...
--
instance IsString GYTime where
    fromString s = fromMaybe (error $ printf "can't parse '%s' as GYTime in ISO8601 format" s) $ gyIso8601ParseM s

instance ParseTime GYTime where
    parseTimeSpecifier _ = parseTimeSpecifier $ Proxy @Time.POSIXTime
    buildTime loc xs     = GYTime <$> buildTime loc xs

instance Swagger.ToParamSchema GYTime where
  toParamSchema _ = mempty
                  & Swagger.type_  ?~ Swagger.SwaggerString
                  & Swagger.format ?~ "string"

instance Swagger.ToSchema GYTime where
  declareNamedSchema p = Swagger.plain $ Swagger.paramSchemaToSchema p
                       & Swagger.type_       ?~ Swagger.SwaggerString
                       & Swagger.description ?~ "This is the posix time in ISO8601 format."
                       & Swagger.format      ?~ "ISO8601"
                       & Swagger.example     ?~ toJSON  ("1970-01-01T00:00:00Z" :: Text)

getCurrentGYTime :: IO GYTime
getCurrentGYTime = GYTime <$> Time.getPOSIXTime

-- |
--
-- >>> timeFromPlutus 12345
-- GYTime 12.345s
--
timeFromPlutus :: Plutus.POSIXTime -> GYTime
timeFromPlutus t = GYTime $ Time.secondsToNominalDiffTime $ fromIntegral t / 1000

-- |
--
-- >>> timeToPlutus $ timeFromPlutus 31415
-- POSIXTime {getPOSIXTime = 31415}
--
timeToPlutus :: GYTime -> Plutus.POSIXTime
timeToPlutus (GYTime t) = round $ 1000 * Time.nominalDiffTimeToSeconds t

-- |
--
-- >>> timeToPOSIX (timeFromPOSIX 12346)
-- 12346s
--
timeToPOSIX :: GYTime -> Time.POSIXTime
timeToPOSIX = coerce

-- |
--
-- >>> timeFromPOSIX 12346
-- GYTime 12346s
--
timeFromPOSIX :: Time.POSIXTime -> GYTime
timeFromPOSIX = coerce

-- |
--
-- >>> gyIso8601Show (timeFromPlutus 0)
-- "1970-01-01T00:00:00Z"
--
gyIso8601Show :: GYTime -> String
gyIso8601Show (GYTime t) = Time.iso8601Show $ Time.posixSecondsToUTCTime t

-- |
--
-- >>> gyIso8601ParseM @Maybe "1970-01-01T00:00:33.333Z"
-- Just (GYTime 33.333s)
--
-- >>> gyIso8601ParseM @Maybe "1970-01-01T00:00:33.333"
-- Nothing
--
gyIso8601ParseM :: MonadFail m => String -> m GYTime
gyIso8601ParseM = fmap (GYTime . Time.utcTimeToPOSIXSeconds) . Time.iso8601ParseM

-- |
--
-- >>> printf "%s\n" $ timeFromPlutus 1000
-- 1970-01-01T00:00:01Z
--
instance PrintfArg GYTime where
    formatArg = formatArg . gyIso8601Show

-- |
--
-- >>> Web.parseUrlPiece @GYTime "1970-01-01T00:00:00Z"
-- Right (GYTime 0s)
--
-- >>> Web.parseUrlPiece @GYTime "1970-01-01T00:00:00"
-- Left "can't parse '1970-01-01T00:00:00' as GYTime in ISO8601 format"
--
instance Web.FromHttpApiData GYTime where
    parseUrlPiece t = maybe
        (Left $ Text.pack $ printf "can't parse '%s' as GYTime in ISO8601 format" t)
        Right
        (gyIso8601ParseM $ Text.unpack t)

-- |
--
-- >>> Web.toUrlPiece $ timeFromPlutus 0
-- "1970-01-01T00:00:00Z"
--
instance Web.ToHttpApiData GYTime where
    toUrlPiece = Web.toUrlPiece . gyIso8601Show

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode $ timeFromPlutus 0
-- "1970-01-01T00:00:00Z"
--
instance Aeson.ToJSON GYTime where
    toJSON = Aeson.toJSON . gyIso8601Show

-- |
--
-- >>> Aeson.eitherDecode @GYTime "\"1970-01-01T00:00:00Z\""
-- Right (GYTime 0s)
--
-- >>> Aeson.eitherDecode @GYTime "\"1970-01-01T00:00:00\""
-- Left "Error in $: can't parse '1970-01-01T00:00:00' as GYTime in ISO8601 format"
--
instance Aeson.FromJSON GYTime where
    parseJSON v = do
        s <- Aeson.parseJSON v
        case gyIso8601ParseM s of
            Just t  -> return t
            Nothing -> fail $ printf "can't parse '%s' as GYTime in ISO8601 format" s
