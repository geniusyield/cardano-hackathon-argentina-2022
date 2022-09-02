{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GeniusYield.Imports (
    module X,
    pattern TODO,
    findFirst,
    decodeUtf8Lenient,
) where

import           Control.Applicative        as X (liftA2)
import           Control.Exception          as X (Exception, catch, throwIO)
import           Control.Monad              as X (ap, foldM, forM, forM_, join,
                                                  unless, when, (>=>))
import           Data.Aeson                 as X (FromJSON (..), ToJSON (..))
import           Data.Bifunctor             as X (first, second)
import           Data.Char                  as X (isAlphaNum, isHexDigit)
import           Data.Coerce                as X (coerce)
import           Data.Either                as X (fromRight)
import           Data.Either.Combinators    as X (rightToMaybe)
import           Data.Foldable              as X (find, foldl', toList)
import           Data.Foldable.WithIndex    as X (ifor_, itoList)
import           Data.Function              as X (on)
import           Data.Functor               as X (void)
import           Data.Functor.Const         as X (Const (..))
import           Data.Functor.Contravariant as X (Contravariant (..))
import           Data.Functor.Identity      as X (Identity (..))
import           Data.List                  as X (minimumBy, maximumBy, sortBy)
import           Data.Map                   as X (Map)
import           Data.Maybe                 as X (fromMaybe, isJust)
import           Data.Proxy                 as X (Proxy (..))
import           Data.Set                   as X (Set)
import           Data.String                as X (IsString (..))
import           Data.Text                  as X (Text)
import           Data.Void                  as X (Void, absurd)
import           GHC.Generics               as X (Generic)
import           GHC.Stack                  as X (CallStack, HasCallStack)
import           Numeric.Natural            as X (Natural)
import           Text.Printf                as X (PrintfArg (..), printf)
import           Witherable                 as X (catMaybes, iwither, mapMaybe,
                                                  wither)

-- Not re-exported.
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as LBS
import           Data.Monoid                (First (..))
import           Data.Text.Encoding.Error   (lenientDecode)
import qualified Data.Text.Lazy             as LT
import           Data.Text.Lazy.Encoding    (decodeUtf8With)
import           GHC.TypeLits               (ErrorMessage (..), TypeError)

-- | Use 'TODO' instead of 'undefined's
pattern TODO :: () => HasCallStack => a
pattern TODO <- (todoMatch -> ())
  where TODO = error "TODO"

{-# DEPRECATED TODO "TODO left in the code" #-}

todoMatch :: a -> ()
todoMatch _ = ()

findFirst :: Foldable f => (a -> Maybe b) -> f a -> Maybe b
findFirst f xs = getFirst (foldMap (coerce f) xs)

-- poisonous instances
-- (the orphan in plutus-ledger-api was removed in Feb 2022)
instance TypeError ('Text "Bad idea") => FromJSON ByteString where
    parseJSON = error "FromJSON @ByteString"

instance TypeError ('Text "Bad idea") => ToJSON ByteString where
    toJSON = error "ToJSON @ByteString"

{- | Decode a 'ByteString' containing UTF-8 encoded text.

Any invalid input bytes will be replaced with the Unicode replacement
character U+FFFD.
-}
decodeUtf8Lenient :: LBS.ByteString -> LT.Text
decodeUtf8Lenient = decodeUtf8With lenientDecode
