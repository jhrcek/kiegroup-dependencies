{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TGF
    ( Coordinate(..)
    , Scope(..)
    , Tgf(..)
    , TgfDepGraph
    , parseDepGraph
    ) where

import           Data.Aeson.TH
import           Data.Attoparsec.Text (Parser, char, choice, decimal,
                                       endOfInput, endOfLine, isEndOfLine,
                                       parseOnly, sepBy, skipMany, space,
                                       string, takeTill, takeWhile, (<?>))
import           Data.Char            (isDigit, toLower)
import           Data.Functor         (($>))
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Txt
import           Prelude              hiding (takeWhile)

data Tgf n e = Tgf
    { nodeDeclarations :: [(Int, n)]
    , edgeDeclarations :: [(Int, Int, e)]
    }

type TgfDepGraph = Tgf Coordinate Scope

parseDepGraph :: Text -> Either String TgfDepGraph
parseDepGraph =
    parseOnly depGraphP

depGraphP :: Parser TgfDepGraph
depGraphP =
    tgfP coordinateP scopeP

tgfP :: Parser n -> Parser e -> Parser (Tgf n e)
tgfP nodeLabelParser edgeLabelParser = Tgf
    <$> (nodeDeclP `sepBy` endOfLine) <* (endOfLine >> char '#' >> endOfLine)
    <*> (edgeDeclP `sepBy` endOfLine) <* (skipMany space >> endOfInput)
  where
    nodeDeclP = (,) <$> dec <*> nodeLabelParser
    edgeDeclP = (,,) <$> dec <*> dec <*> edgeLabelParser
    dec = decimal <* space

data Scope = Compile | Provided | Runtime | System | Test
    deriving (Eq, Ord, Show)

scopeP :: Parser Scope
scopeP = choice
    [ string "compile" $> Compile
    , string "provided" $> Provided
    , string "runtime" $> Runtime
    , string "system" $> System
    , string "test" $>  Test
    ]

{-| Maven Coordinates as described in https://maven.apache.org/pom.html#Maven_Coordinates -}
data Coordinate = Coordinate
    { cGroupId    :: Text
    , cArtifactId :: Text
    , cPackaging  :: Text
    , cQualifier  :: Maybe Text
    , cVersion    :: Text
    } deriving (Eq, Ord)

instance Show Coordinate where
    show (Coordinate grp art pac mayQualifier ver) =
        Txt.unpack $ Txt.intercalate ":" fields
      where
        fields = grp : art : pac : case mayQualifier of
            Just qual -> [qual, ver]
            Nothing   -> [      ver]

coordinateP :: Parser Coordinate
coordinateP = do
    groupId <- field <?> "groupId"
    artifactId <- field <?> "artifactId"
    packaging <- field <?> "packaging"
    rest <- takeTill isEndOfLine <?> "rest"
    coord <- case Txt.splitOn ":" rest of
        [qualifier, version, _scope] -> return $ Coordinate groupId artifactId packaging (Just qualifier) version
        [version, _scope]            -> return $ Coordinate groupId artifactId packaging Nothing version
        [version]                    -> return $ Coordinate groupId artifactId packaging Nothing version
        unexpected                   -> fail $ "Unexpected fields" ++ show unexpected
    either fail return $ validateCoordinate coord
  where
    field = takeWhile (/=':') <* char ':'

validateCoordinate :: Coordinate -> Either String Coordinate
validateCoordinate c =
    validateVersion c >>= validatePackaging
  where
    validateVersion c' =
        if Txt.any isDigit (cVersion c') || cVersion c' == "jdk"
            then return c'
            else Left $ "I was expecting verision to contain at least one digit in " ++ show c'
    validatePackaging c' =
        if cPackaging c' `Set.member` knownPackagings
            then return c'
            else Left $ "Urecognized dependency packaging in " ++ show c'

knownPackagings :: Set Text
knownPackagings = Set.fromList
    ["bundle"
    ,"eclipse-feature"
    ,"eclipse-plugin"
    ,"eclipse-repository"
    ,"eclipse-test-plugin"
    ,"gwt-lib"
    ,"gwt-app"
    ,"jar"
    ,"kjar"
    ,"maven-archetype"
    ,"maven-module"
    ,"maven-plugin"
    ,"pom"
    ,"takari-maven-plugin"
    ,"tar.gz"
    ,"test-jar"
    ,"war"
    ,"xml"
    ,"zip"
    ]

$(deriveJSON defaultOptions{ fieldLabelModifier = map toLower . take 2 . drop 1
                           , omitNothingFields = True
                           } ''Coordinate)

$(deriveJSON defaultOptions{ fieldLabelModifier = map toLower
                            , constructorTagModifier = \t -> [toLower (head t)]
                            } ''Scope)
