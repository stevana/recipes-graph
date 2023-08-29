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
  [ "<html>"
  , "  <head>"
  , "    <title>Recipes</title>"
  , "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />"
  , "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />"
  , "    <link rel=\"stylesheet\" href=\"style.css\" />"
  , "  </head>"
  , "  <body>"
  ]

menu :: Queries -> String -> String -> Text
menu (Queries qs) selectedQname selectedQparam = T.unlines $
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
      flip map qs $ \(SomeQuery qname _q) -> T.concat $
        [ "<li><h5 class=\"category\">" <> T.pack qname <> "</h5>"
        , "  <ul>"
        -- , "    <button onclick=\"alert('test')\"><li><a href=" <> T.pack qname <> "-Any.html>Any</a></li></button>"
        , "    <button><li><a href=" <> T.pack qname <> "-Any.html>Any</a></li></button>"
        ] <> (

        flip map universe $ \qparam ->
          let _ = _q undefined [qparam] in -- NOTE: Only used for type inferance of qparam.
            "<li><a href=" <> T.pack qname <> "-" <> text qparam <> ".html>" <>
              (
              if show qparam == selectedQparam
              then "<u>" <> T.replace "_" " " (text qparam) <> "</u>"
              else T.replace "_" " " (text qparam)
              )
              <> "</a></li>" ) <>
        ["</li></ul>"]

body :: [Recipe] -> Text
body = T.unlines . map displayRecipe

displayRecipe :: Recipe -> Text
displayRecipe (Recipe name meals mKitchen ingredients_ mInstructions) = T.unlines $
  [ "<h3>" <> name <> "</h3>" ] ++
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
footer = "</body></html>"

generatePage :: FilePath -> Queries -> String -> String -> [Recipe] -> IO ()
generatePage fp queries qname qparam recipes =
  let
    txt = header <> menu queries qname qparam <> body recipes <> footer
  in
    T.writeFile fp txt

generatePages :: FilePath -> Connection -> Queries -> IO ()
generatePages distDir conn queries@(Queries qs) = do
  allRecipes <- queryAllRecipes conn
  generatePage (distDir </> "index" <.> "html") queries "All" "All" allRecipes
  forM_ qs $ \(SomeQuery qname q) -> do
    putStrLn $ "Generating page for: " <> qname
    anyRecipes <- q conn universe
    let fp  = distDir </> qname <> "-Any.html"
    generatePage fp queries qname "Any" anyRecipes
    forM_ universe $ \qparam -> do
      recipes <- q conn [qparam]
      let fp' = distDir </> qname <> "-" <> show qparam <.> "html"
      generatePage fp' queries qname (show qparam) recipes
