module Util  (filepathToText, filepathToString) where

import Data.Text (Text)
import qualified Data.Text as Text
import Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as OSPath
import Prelude hiding (FilePath)

filepathToText :: FilePath -> Text
filepathToText = either (error . show) id . OSPath.toText

filepathToString :: FilePath -> String
filepathToString = Text.unpack . filepathToText
