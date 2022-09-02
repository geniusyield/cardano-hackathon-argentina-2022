module GeniusYield.Utils (fieldNamePrefixStrip2, fieldNamePrefixStrip3, fieldNamePrefixStripN) where

import Data.Char (toLower)

-- | @fieldNamePrefixStrip3 "msnNumber" == "number"@
fieldNamePrefixStrip2 :: String -> String
fieldNamePrefixStrip2 = fieldNamePrefixStripN 2

-- | @fieldNamePrefixStrip3 "muAssets" == "assets"@
fieldNamePrefixStrip3 :: String -> String
fieldNamePrefixStrip3 = fieldNamePrefixStripN 3

-- | Strip n characters from a field name and lower case the first character (if any) of the result.
fieldNamePrefixStripN :: Int -> String -> String
fieldNamePrefixStripN n fldName = case drop n fldName of x : xs -> toLower x : xs; [] -> []
