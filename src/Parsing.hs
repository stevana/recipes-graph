{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Parsing where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)

------------------------------------------------------------------------

data Recipe = Recipe
  { id           :: Text
  , meal         :: [Meal]
  , kitchen      :: Maybe Kitchen
  , ingredients  :: [Text]
  , instructions :: Maybe [Text]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Meal = Breakfast | Brunch | Lunch | Starter | Dinner | Side
          | Dessert | Picnic
  deriving (Eq, Show, Enum, Bounded, Generic, ToJSON, FromJSON)

data Kitchen = Asian | Italian | French | Central_European | Mexican
             | Middle_Eastern | Greek
  deriving (Eq, Show, Enum, Bounded, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------

class Finite a where
  universe :: [a]

instance (Finite a, Finite b) => Finite (a, b) where
  universe = [ (x, y) | x <- universe, y <- universe ]

instance Finite Kitchen where
  universe = enumFrom minBound

instance Finite Meal where
  universe = enumFrom minBound

------------------------------------------------------------------------

parseRecipes :: FilePath -> IO [Recipe]
parseRecipes fp = Yaml.decodeFileThrow fp
