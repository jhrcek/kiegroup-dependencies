{-# LANGUAGE OverloadedStrings #-}
module TGF (parseDependencyTree) where

import Data.List (span)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Txt
import Data.Text.Read (decimal)
import Data.Tree

parseDependencyTree :: Text -> Tree Text
parseDependencyTree tgf =
    fmap (\id -> fromMaybe "MISSING GAV" $ Map.lookup id nodeLabels) treeOfIds
  where
    (nodeLines, hashAndEdgeLines) = span (/= "#") $ Txt.lines tgf

    rootId  :: Int
    rootId = fst . readNodeLine $ head nodeLines

    edges :: [(Int, Int)]
    edges  = map readEdgeLine $ tail hashAndEdgeLines

    treeOfIds :: Tree Int
    treeOfIds = buildTree rootId edges

    nodeLabels :: Map Int Text
    nodeLabels = readNodeLines nodeLines


buildTree :: Int -> [(Int, Int)] -> Tree Int
buildTree rootId edges =
  let childrenOfRoot = map snd $ filter ((==rootId) . fst) edges
  in Node rootId . fmap (\child -> buildTree child edges) $ childrenOfRoot


readNodeLines :: [Text] -> Map Int Text
readNodeLines =
  Map.fromList . map readNodeLine


readNodeLine :: Text -> (Int, Text)
readNodeLine line =
  case Txt.words line of
    (nodeIdText:rest) ->
      let nodeId = parseOrError nodeIdText
          nodeText = Txt.unwords rest
      in ( nodeId , nodeText )
    _ -> error $ "Unexpected format of node line: " ++ Txt.unpack line


readEdgeLine :: Text -> (Int, Int)
readEdgeLine line =
  case Txt.words line of
    [fromIdText, toIdText, _edgeText] ->
        let fromId = parseOrError fromIdText
            toId =  parseOrError toIdText
        in ( fromId, toId )
    _ -> error $ "Unexpected format of edge line: " ++ Txt.unpack line


parseOrError :: Text -> Int
parseOrError = either (error . ("Failed to parse Int from " ++ )) fst . decimal
