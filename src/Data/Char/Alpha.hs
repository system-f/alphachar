{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Data.Char.Alpha(
-- * Upper-case
  AlphaUpper
, HasAlphaUpper(alphaUpper)
, AsAlphaUpper(_AlphaUpper)
, parseAlphaUpper
-- * Lower-case
, AlphaLower
, HasAlphaLower(alphaLower)
, AsAlphaLower(_AlphaLower)
, parseAlphaLower
-- * Any case
, Alpha(UpperAlpha, LowerAlpha)
, HasAlpha(alpha)
, AsAlpha(_Alpha, _UpperAlpha, _LowerAlpha)
, parseAlpha
-- * Accessory
, upperlower
, lowerupper
) where

import Control.Applicative((<$>), (<|>), empty)
import Control.Category((.), id)
import Control.Lens(makeClassy, makeClassyPrisms, from, iso, prism', (^?), Iso', Prism')
import Control.Monad(Monad((>>=), return))
import Data.Char(Char, toLower, toUpper)
import Data.Eq(Eq)
import Data.Foldable(elem)
import Data.Maybe(Maybe(Just, Nothing), maybe)
import Data.Ord(Ord)
import Prelude(Show)
import Text.Parser.Char(CharParsing, anyChar)
import Text.Parser.Combinators((<?>), try)

-- $setup
--
-- >>> import Text.Parsec(parse)
-- >>> import Control.Lens((#), isn't, _Right)

-- | An upper-case character between @'A'@ and @'Z'@.
newtype AlphaUpper =
  AlphaUpper Char
  deriving (Eq, Ord, Show)

makeClassy ''AlphaUpper

class AsAlphaUpper a where
  _AlphaUpper ::
    Prism'
      a
      AlphaUpper

instance AsAlphaUpper AlphaUpper where
  _AlphaUpper =
    id

instance AsAlphaUpper Char where
  _AlphaUpper =
    prism'
      (\(AlphaUpper c) -> c)
      (\c ->  if c `elem` ['A'..'Z']
                then
                  Just (AlphaUpper c)
                else
                  Nothing)

-- | Parse an upper-case alpha character.
--
-- >>> parse parseAlphaUpper "parseAlphaUpper" "A"
-- Right (AlphaUpper 'A')
--
-- >>> parse parseAlphaUpper "parseAlphaUpper" "Abc"
-- Right (AlphaUpper 'A')
--
-- >>> isn't _Right (parse parseAlphaUpper "parseAlphaUpper" "a")
-- True
--
-- >>> isn't _Right (parse parseAlphaUpper "parseAlphaUpper" "")
-- True
--
-- >>> isn't _Right (parse parseAlphaUpper "parseAlphaUpper" "0")
-- True
parseAlphaUpper ::
  (Monad f, CharParsing f) =>
  f AlphaUpper
parseAlphaUpper =
  try (anyChar >>= \c -> maybe empty return (c ^? _AlphaUpper)) <?> "AlphaUpper"

-- | A lower-case character between @'a'@ and @'z'@.
newtype AlphaLower =
  AlphaLower Char
  deriving (Eq, Ord, Show)

makeClassy ''AlphaLower

class AsAlphaLower a where
  _AlphaLower ::
    Prism'
      a
      AlphaLower

instance AsAlphaLower AlphaLower where
  _AlphaLower =
    id

instance AsAlphaLower Char where
  _AlphaLower =
    prism'
      (\(AlphaLower c) -> c)
      (\c ->  if c `elem` ['a'..'z']
                then
                  Just (AlphaLower c)
                else
                  Nothing)

-- | Parse a lower-case alpha character.
--
-- >>> parse parseAlphaLower "parseAlphaLower" "a"
-- Right (AlphaLower 'a')
--
-- >>> parse parseAlphaLower "parseAlphaLower" "aBC"
-- Right (AlphaLower 'a')
--
-- >>> isn't _Right (parse parseAlphaLower "parseAlphaLower" "B")
-- True
--
-- >>> isn't _Right (parse parseAlphaLower "parseAlphaLower" "")
-- True
--
-- >>> isn't _Right (parse parseAlphaLower "parseAlphaLower" "0")
-- True
parseAlphaLower ::
  (Monad f, CharParsing f) =>
  f AlphaLower
parseAlphaLower =
  try (anyChar >>= \c -> maybe empty return (c ^? _AlphaLower)) <?> "AlphaLower"

-- | Either a lower-case character between @'a'@ and @'z'@ or an upper-case character between @'A'@ and @'Z'@.
data Alpha =
  UpperAlpha AlphaUpper
  | LowerAlpha AlphaLower
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Alpha
makeClassy ''Alpha

-- | Parse an upper-case or lower-case character.
--
-- >>> parse parseAlpha "parseAlpha" "a"
-- Right (LowerAlpha (AlphaLower 'a'))
--
-- >>> parse parseAlpha "parseAlpha" "aBC"
-- Right (LowerAlpha (AlphaLower 'a'))
--
-- >>> parse parseAlpha "parseAlpha" "A"
-- Right (UpperAlpha (AlphaUpper 'A'))
--
-- >>> parse parseAlpha "parseAlpha" "Abc"
-- Right (UpperAlpha (AlphaUpper 'A'))
--
-- >>> isn't _Right (parse parseAlpha "parseAlpha" "0")
-- True
parseAlpha ::
  (Monad f, CharParsing f) =>
  f Alpha
parseAlpha =
   UpperAlpha <$> parseAlphaUpper <|> LowerAlpha <$> parseAlphaLower

instance AsAlphaLower Alpha where
  _AlphaLower =
    _LowerAlpha . _AlphaLower

instance AsAlphaUpper Alpha where
  _AlphaUpper =
    _UpperAlpha . _AlphaUpper

-- | Isomorphism from upper to lower.
--
-- >>> (upperlower #) <$> ('a' ^? _AlphaLower)
-- Just (AlphaUpper 'A')
--
-- >>> (upperlower #) <$> ('A' ^? _AlphaLower)
-- Nothing
--
-- >>> (upperlower #) <$> ('3' ^? _AlphaLower)
-- Nothing
upperlower ::
  Iso'
    AlphaUpper
    AlphaLower
upperlower =
  iso
    (\(AlphaUpper c) -> AlphaLower (toLower c))
    (\(AlphaLower c) -> AlphaUpper (toUpper c))

-- | Isomorphism from lower to upper.
--
-- >>> (lowerupper #) <$> ('A' ^? _AlphaUpper)
-- Just (AlphaLower 'a')
--
-- >>> (lowerupper #) <$> ('a' ^? _AlphaUpper)
-- Nothing
--
-- >>> (lowerupper #) <$> ('3' ^? _AlphaUpper)
-- Nothing
lowerupper ::
  Iso'
    AlphaLower
    AlphaUpper
lowerupper =
  from upperlower
