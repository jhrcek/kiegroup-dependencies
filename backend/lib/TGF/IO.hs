module TGF.IO
   ( loadDependencyTree
   ) where

import           Control.Monad
import           Data.Bifunctor            (first)
import           Data.Either
import qualified Data.Text.IO              as Txt
import           Filesystem.Path.CurrentOS as OSPath
import           Prelude                   hiding (FilePath)
import           TGF                       (TgfDepGraph, parseDepGraph)
import           Util                      (filepathToString)


loadDependencyTree :: FilePath -> IO (Either String TgfDepGraph)
loadDependencyTree tgfFile = do
    let strFile = filepathToString tgfFile
        addFilenameToError err = "Failed to parse contents of " ++ strFile ++ "; Parse error '" ++ err ++ "'"
    contents <- Txt.readFile strFile
    return . first addFilenameToError $ parseDepGraph contents
