module Util  (filepathToText, filepathToString) where

import Data.Text (Text, unpack)
import Prelude hiding (FilePath)
import Turtle (FilePath)
import Turtle.Format (format, fp)

filepathToText :: FilePath -> Text
filepathToText = format fp

filepathToString :: FilePath -> String
filepathToString = unpack . filepathToText
