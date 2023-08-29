module Utils where

import Data.Text (Text)
import qualified Data.Text as T

------------------------------------------------------------------------

text :: Show a => a -> Text
text x = T.pack (show x)
