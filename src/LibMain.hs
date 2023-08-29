module LibMain where

import System.Directory
import System.FilePath

import Generate (generatePages)
import Parsing (parseRecipes)
import Paths_recipes_graph (getDataFileName)
import Queries (allQueries, insertRecipe, withDB)

------------------------------------------------------------------------
-- * Constants

-- | The input data directory, this is where the recipes, style sheets and
--   javascript are stored.
dATA_DIR :: FilePath
dATA_DIR = "data"

rECIPES_FILE :: FilePath
rECIPES_FILE = "recipes.yaml"

cSS_FILE :: FilePath
cSS_FILE = "style.css"

jS_FILE :: FilePath
jS_FILE = "script.js"

-- | The output data directory, this is where the generated database and html
-- will be stored.
dIST_DIR :: FilePath
dIST_DIR = "dist"

dB_FILE :: FilePath
dB_FILE = "recipes.sqlite"

------------------------------------------------------------------------

libMain :: IO ()
libMain = do
  createDirectoryIfMissing True dIST_DIR

  css <- getDataFileName (dATA_DIR </> cSS_FILE)
  copyFile css (dIST_DIR </> cSS_FILE)

  js <- getDataFileName (dATA_DIR </> jS_FILE)
  copyFile js (dIST_DIR </> jS_FILE)

  yaml <- getDataFileName (dATA_DIR </> rECIPES_FILE)
  recipes <- parseRecipes yaml

  withDB (dIST_DIR </> dB_FILE) $ \conn -> do
    mapM_ (insertRecipe conn) recipes
    generatePages dIST_DIR conn allQueries
