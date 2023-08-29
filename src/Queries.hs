{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries where

import Control.Monad
import Data.Aeson (decodeStrict, encode)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.List (intersect)
import qualified Data.Text as T
import Data.Text.Encoding
import Database.SQLite.Simple

import Parsing
import Utils

------------------------------------------------------------------------
-- * Schema

withDB :: FilePath -> (Connection -> IO a) -> IO a
withDB fp k = withConnection fp $ \conn -> do
  execute_ conn createNodesTable
  execute_ conn "CREATE INDEX IF NOT EXISTS id_idx ON nodes(id)"
  execute_ conn createEdgesTable
  execute_ conn "CREATE INDEX IF NOT EXISTS source_idx ON edges(source)"
  execute_ conn "CREATE INDEX IF NOT EXISTS target_idx ON edges(target)"
  k conn
  where
    createNodesTable = Query $ T.unlines
      [ "CREATE TABLE IF NOT EXISTS nodes ("
      , "   body TEXT,"
      , "   id   TEXT GENERATED ALWAYS AS (json_extract(body, '$.id')) VIRTUAL NOT NULL UNIQUE"
      , ");"
      ]
    createEdgesTable = Query $ T.unlines
      [ "CREATE TABLE IF NOT EXISTS edges ("
      , "    source     TEXT,"
      , "    target     TEXT,"
      , "    properties TEXT,"
      , "    UNIQUE(source, target, properties) ON CONFLICT REPLACE,"
      , "    FOREIGN KEY(source) REFERENCES nodes(id),"
      , "    FOREIGN KEY(target) REFERENCES nodes(id)"
      , ");"
      ]

------------------------------------------------------------------------
-- * Insertion

insertRecipe :: Connection -> Recipe -> IO ()
insertRecipe conn r = do
  execute conn "INSERT INTO nodes VALUES(json(?))" (Only (encode r))
  forM_ (meal r) $ \m -> do
    execute conn "INSERT INTO edges VALUES(?, ?, ?)" (Parsing.id r, text m, show EatenAt)
  forM_ (diet r) $ \d -> do
    execute conn "INSERT INTO edges VALUES(?, ?, ?)" (Parsing.id r, text d, show EatenBy)

data EatenAt = EatenAt
  deriving (Show)

data EatenBy = EatenBy
  deriving (Show)

------------------------------------------------------------------------
-- * Queries

newtype Queries = Queries { unQueries :: [SomeQuery] }

data SomeQuery = forall a. (Show a, Finite a) =>
                 SomeQuery String (Connection -> [a] -> IO [Recipe])

queryName :: SomeQuery -> String
queryName (SomeQuery qname _q) = qname

allQueries :: Queries
allQueries = Queries [ SomeQuery "Kitchen" queryKitchen
                     , SomeQuery "Meal"    queryMeal
                     , SomeQuery "Diet"    queryDiet
                     ]

queryAllRecipes :: Connection -> IO [Recipe]
queryAllRecipes conn = do
  rs <- query_ conn "SELECT body FROM nodes"
  return (map (fromJust . decodeStrict . encodeUtf8 . T.concat) rs)

queryKitchen :: Connection -> [Kitchen] -> IO [Recipe]
queryKitchen conn kitchens = do
  let clause = foldr (\k ih -> "json_extract(body, '$.kitchen') = '" <>
                               T.pack (show k) <> "' OR " <> ih)
                     "FALSE" kitchens
      q      = Query ("SELECT body FROM nodes WHERE " <> clause)
  rs <- query_ conn q
  return (map (fromJust . decodeStrict . encodeUtf8 . T.concat) rs)

queryMeal :: Connection -> [Meal] -> IO [Recipe]
queryMeal conn meals = do
  let clause = foldr (\m ih -> "target = '" <> text m <> "' OR " <> ih) "FALSE" meals
      q = Query ("SELECT DISTINCT nodes.body FROM edges LEFT JOIN \
                 \ nodes ON edges.source = nodes.id WHERE \
                 \ edges.properties = 'EatenAt' AND " <> clause)
  rs <- query_ conn q
  return (map (fromJust . decodeStrict . encodeUtf8 . T.concat) rs)

queryDiet :: Connection -> [Diet] -> IO [Recipe]
queryDiet conn diets = do
  let clause = foldr (\m ih -> "target = '" <> text m <> "' OR " <> ih) "FALSE" diets
      q = Query ("SELECT DISTINCT nodes.body FROM edges LEFT JOIN \
                 \ nodes ON edges.source = nodes.id WHERE \
                 \ edges.properties = 'EatenBy' AND " <> clause)
  rs <- query_ conn q
  return (map (fromJust . decodeStrict . encodeUtf8 . T.concat) rs)

powerQuery :: Queries -> Queries
powerQuery (Queries qs0) = Queries (go qs0)
  where
    go :: [SomeQuery] -> [SomeQuery]
    go [] = []
    go (q : qs)
      = -- q
      -- XXX: this equality should never hold unless there are duplicates in queries?
      concatMap (\q' -> if queryName q == queryName q' then [] else [intersectSomeQuery q q'])
                  (go qs)
      ++ go qs

intersectSomeQuery :: SomeQuery -> SomeQuery -> SomeQuery
intersectSomeQuery (SomeQuery qname q) (SomeQuery qname' q') =
  SomeQuery (qname <> "-" <> qname') $ \conn xys -> do
    let (xs, ys) = unzip xys
    rs  <- q  conn xs
    rs' <- q' conn ys
    return (rs `intersect` rs')

------------------------------------------------------------------------
-- * Debugging

queryNodes :: Connection -> IO [[Text]]
queryNodes conn = query_ conn "SELECT body FROM nodes"

querySchema :: Connection -> IO [[SQLData]]
querySchema conn = query_ conn "SELECT * FROM sqlite_schema"
