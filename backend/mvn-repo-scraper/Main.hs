{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where
import           Coordinate
import           Data.Aeson              (Value, eitherDecode, withObject, (.:))
import           Data.Aeson.Types        (Parser, parseEither)
import           Data.ByteString.Char8   (ByteString)
import qualified Data.ByteString.Lazy    as LBS
import           Data.Maybe              (listToMaybe)
import qualified Data.Text               as Txt
import           Network.HTTP.Client.TLS (setGlobalManager)
import           Network.HTTP.Conduit    (Request, Response, newManager,
                                          parseRequest, tlsManagerSettings)
import           Network.HTTP.Simple     (getResponseBody,
                                          getResponseStatusCode, httpBS)
import           System.Environment      (getArgs)
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match

main :: IO ()
main = do
    coords <- either error (map snd) <$> (readCoordinates =<< getDepGraphFile)
    rqs <- mapM toMvnRepoUrl $ take 10 coords

    setGlobalManager =<< newManager tlsManagerSettings
    mapM scrapeArtifactInfo rqs >>= print

toMvnRepoUrl :: Coordinate -> IO Request
toMvnRepoUrl c =
    parseRequest $ Txt.unpack $ Txt.intercalate "/" ["https://mvnrepository.com/artifact", cGroupId c, cArtifactId c, cVersion c]

getDepGraphFile :: IO FilePath
getDepGraphFile = do
    args <- getArgs
    case args of
        [file] -> return file
        _      -> error "Please provide path to file dependency-graph.json"

scrapeArtifactInfo :: Request -> IO ArtifactStatus
scrapeArtifactInfo req =
    getArtifactInfo <$> httpBS req

getArtifactInfo :: Response ByteString -> ArtifactStatus
getArtifactInfo resp =
  case getResponseStatusCode resp of
    404   -> NotFound
    200   -> extractInfo (getResponseBody resp)
    other -> error $ "Unexpected response code" ++ show other

extractInfo :: ByteString -> ArtifactStatus
extractInfo body = Found{..}
  where
    tags = parseTags body
    description = fromTagText . (!!1) $ dropWhile (not . tagOpenAttrLit "div" ("class","im-description")) tags
    newVersion = maybeTagText =<< (listToMaybe . drop 4  $ dropWhile (/=TagText "New Version") tags)

data ArtifactStatus
    = NotFound
    | Found { description :: ByteString
            , newVersion  :: Maybe ByteString
            }
    deriving Show


parseCoordinates :: Value -> Parser [(Int, Coordinate)]
parseCoordinates = withObject "graph" $ \obj -> obj .: "nodes"


readCoordinates :: FilePath -> IO (Either String [(Int,Coordinate)])
readCoordinates depGraphJson = do
     content <- LBS.readFile depGraphJson
     return $ parseEither parseCoordinates =<< eitherDecode content
