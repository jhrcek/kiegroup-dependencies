module TGF.IO
   ( loadDepsFromFile
   , loadTgfFromFile
   ) where

import           Control.Monad
import           Data.Bifunctor            (first)
import           Data.Either
import qualified Data.Text.IO              as Txt
import           Filesystem.Path.CurrentOS as OSPath
import           Prelude                   hiding (FilePath)
import qualified TGF
import           Util                      (filepathToString)

loadDepsFromFile :: FilePath -> IO (Either String TGF.Deps)
loadDepsFromFile tgfFile = do
  eitherTgf <- loadTgfFromFile tgfFile
  return $ eitherTgf >>= TGF.toDeps

loadTgfFromFile :: FilePath -> IO (Either String TGF.TGF)
loadTgfFromFile tgfFile = do
    let strFile = filepathToString tgfFile
        addFilenameToError err = "Failed to parse contents of " ++ strFile ++ "; Parse error '" ++ err ++ "'"
    contents <- Txt.readFile strFile
    return . first addFilenameToError $ TGF.parseTGF contents
