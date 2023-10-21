{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Parsing where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)

import Utils

------------------------------------------------------------------------

data Recipe = Recipe
  { id           :: Text
  , meal         :: [Meal]
  , kitchen      :: Maybe Kitchen
  , diet         :: [Diet]
  , ingredients  :: [Text]
  , instructions :: Maybe [Text]
  , source       :: Maybe Text
  , url          :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Meal
  = Breakfast
  | Brunch
  | Lunch
  | Starter
  | Dinner
  | Side
  | Dessert
  | Picnic
  deriving (Eq, Show, Enum, Bounded, Generic, ToJSON, FromJSON, Finite, Display)

data Kitchen
  = Asian
  | Central_European
  | Northern_European
  | French
  | Greek
  | Italian
  | Mexican
  | Middle_Eastern
  | American
  | African
  deriving (Eq, Show, Enum, Bounded, Generic, ToJSON, FromJSON, Finite, Display)

data Diet
  = Meat
  | Vegetarian
  | Vegan
  | Gluten
  deriving (Eq, Show, Enum, Bounded, Generic, ToJSON, FromJSON, Finite, Display)

------------------------------------------------------------------------

parseRecipes :: FilePath -> IO [Recipe]
parseRecipes fp = Yaml.decodeFileThrow fp
-- XXX: decodeFileWithWarnings :: FromJSON a => FilePath -> IO (Either ParseException ([Warning], a))
