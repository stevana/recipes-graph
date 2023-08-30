module Utils where

import Data.Text (Text)
import qualified Data.Text as T

import Parsing

------------------------------------------------------------------------

for :: [a] -> (a -> b) -> [b]
for = flip map

text :: Show a => a -> Text
text x = T.pack (show x)

------------------------------------------------------------------------

class Finite a where
  universe :: [a]

instance (Finite a, Finite b) => Finite (a, b) where
  universe = [ (x, y) | x <- universe, y <- universe ]

instance Finite Kitchen where
  universe = enumFrom minBound

instance Finite Meal where
  universe = enumFrom minBound

instance Finite Diet where
  universe = enumFrom minBound

instance Finite Bool where
  universe = [True, False]

powerset :: (Monoid (m a), Applicative m) => [a] -> [m a]
powerset = go
  where
    go []       = [mempty]
    go (x : xs) = let ps = go xs in ps <> map (<> pure x) ps

finitePowerset :: (Finite a, Monoid (m a), Applicative m) => [m a]
finitePowerset = powerset universe
