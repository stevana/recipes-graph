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
  , "<ul>"
  ] ++ lis ++
  [ "</ul>"
  , "</nav>"
  , "</header>"
  , "<br />"
  , "<hr />"
  ]
  where
    lis :: [Text]
    lis =
      for qs $ \(SomeQuery qname _q) -> T.concat $
        [ "<li><h5 class=\"category\">" <> T.pack qname <> "</h5>"
        , "  <ul>"
        ] <> (

        for universe $ \qparam ->
          let _ = _q undefined [qparam] in -- NOTE: Only used for type inferance of qparam.
            -- "<button id=\"" <> T.pack qname <> "-" <> text qparam <> "\"><li><a href=" <> T.pack qname <> "-" <> text qparam <> ".html>" <>
            "<li><button class=\"" <> T.pack qname <> "\" id=\"" <> text qparam <> "\">" <>
            T.replace "_" " " (text qparam) <>
            "</button></li>" ) <>
        ["</li></ul>"]

body :: [Recipe] -> Text
body = T.unlines . map displayRecipe

displayRecipe :: Recipe -> Text
displayRecipe (Recipe name meals mKitchen diets ingredients_ mInstructions) = T.unlines $
  [ "<h3>" <> name <> (if Vegan `elem` diets then " <font color=green>Ⓥ</font>" else "") <> "</h3>" ] ++
  [ maybe "" (\k -> "<i>(" <> T.replace "_" " " (text k) <>
               if null meals
               then ""
               else "; " <> T.intercalate ", " (map (T.toLower . text) meals) <>
               ")</i>") mKitchen ] ++
  ["<ul>"] ++
  map (\i -> "<li>" <> i <> "</li>") ingredients_ ++
  ["</ul>"] ++
  maybe [""] (\instructions_ ->
                "<ol>" : map (\i -> "<li>" <> i <> "</li>") instructions_ ++ ["</ol>"])
        mInstructions

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
      let fp' = distDir </> qname <> "-" <> show qparam <.> "html"
      generatePage fp' queries recipes
