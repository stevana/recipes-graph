{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Data.Text (Text)
import qualified Data.Text as T

------------------------------------------------------------------------

for :: [a] -> (a -> b) -> [b]
for = flip map

text :: Show a => a -> Text
text x = T.pack (show x)

------------------------------------------------------------------------

class Finite a where
  universe :: [a]

  default universe :: (Enum a, Bounded a) => [a]
  universe = enumFrom (minBound :: a)

instance (Finite a, Finite b) => Finite (a, b) where
  universe = [ (x, y) | x <- universe, y <- universe ]

------------------------------------------------------------------------

class Display a where
  display :: a -> [Text]

  default display :: Show a => a -> [Text]
  display x = [text x]

instance (Display a, Display b) => Display (a, b) where
  display (x, y) = display x ++ display y
