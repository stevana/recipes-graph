{-# LANGUAGE OverloadedStrings #-}

module Generate where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.SQLite.Simple (Connection)
import System.FilePath ((</>), (<.>))

import Parsing
import Queries
import Utils

------------------------------------------------------------------------

header :: Text
header = T.unlines
  [ "<!doctype html>"
  , "<html lang=\"en\">"
  , "  <head>"
  , "    <title>Recipes</title>"
  , "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />"
  , "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />"
  , "    <link rel=\"stylesheet\" href=\"style.css\" />"
  , "  </head>"
  , "  <body>"
  ]

menu :: Queries -> Text
menu (Queries qs) = T.unlines $
  [ "<header>"
  , "<h1>Recipes</h1>"
  , "<nav id=\"nav\" class=\"menu\">"
  , ul lis
  , "</nav>"
  , "</header>"
  , "<br />"
  , "<hr />"
  ]
  where
    lis :: [Text]
    lis =
      for qs $ \(SomeQuery qname _q) -> T.concat $
        [ "<h5 class=\"category\">" <> T.pack qname <> "</h5>"
        , ul $ for universe $ \qparam ->
            let _ = _q undefined [qparam] in -- NOTE: Only used for type inferance of qparam.
            "<button class=\"" <> T.pack qname <> "\" id=\"" <> T.concat (display qparam) <> "\">" <>
            T.replace "_" " " (T.concat (display qparam)) <> "</button>"
        ]

body :: [Recipe] -> Text
body [] = "<p>No recipes found, try changing some filters.</p>"
body rs = T.unlines (map displayRecipe rs)

displayRecipe :: Recipe -> Text
displayRecipe (Recipe name meals mKitchen diets ingredients_ mInstructions mSource mUrl) = T.unlines $
  [ h3 (name <> (if Vegan `elem` diets then " <font color=green>Ⓥ</font>" else ""))
  , italic $ parenthesis $
      maybe mealsText (\k -> T.replace "_" " " (text k) <>
                        if null meals
                        then ""
                        else "; " <> mealsText
                        ) mKitchen
  , ul ingredients_
  , maybe "" ol mInstructions
  , displaySourceUrl mSource mUrl
  ]
  where
    mealsText = T.intercalate ", " (map (T.toLower . text) meals)

    displaySourceUrl :: Maybe Text -> Maybe Text -> Text
    displaySourceUrl Nothing    Nothing     = ""
    displaySourceUrl (Just src) Nothing     = "by " <> src
    displaySourceUrl Nothing    (Just href) = "by <a href=\"" <> href <> "\">anoymous</a>"
    displaySourceUrl (Just src) (Just href) = "by <a href=\"" <> href <> "\">" <> src <> "</a>"

h3 :: Text -> Text
h3 t = "<h3>" <> t <> "</h3>"

italic :: Text -> Text
italic t = "<i>" <> t <> "</i>"

parenthesis :: Text -> Text
parenthesis t = "(" <> t <> ")"

ul :: [Text] -> Text
ul ils = "<ul>\n" <> T.unlines (map (\il -> "  <li>" <> il <> "</li>") ils) <> "</ul>"

ol :: [Text] -> Text
ol ils = "<ol>\n" <> T.unlines (map (\il -> "  <li>" <> il <> "</li>") ils) <> "</ol>"

footer :: Text
footer = "<script src=\"script.js\"></script></body></html>"

generatePage :: FilePath -> Queries -> [Recipe] -> IO ()
generatePage fp queries recipes =
  let
    txt = header <> menu queries <> body recipes <> footer
  in
    T.writeFile fp txt

generatePages :: FilePath -> Connection -> Queries -> IO ()
generatePages distDir conn queries = do
  allRecipes <- queryAllRecipes conn
  generatePage (distDir </> "index" <.> "html") queries allRecipes
  let Queries ps = powerQuery queries
  forM_ ps $ \(SomeQuery qname q) -> do
    putStrLn $ "Generating page for: " <> qname
    forM_ universe $ \qparam -> do
      recipes <- q conn [qparam]
      let fp = distDir </> qname <> "-" <> T.unpack (T.intercalate "-" (display qparam)) <.> "html"
      generatePage fp queries recipes
