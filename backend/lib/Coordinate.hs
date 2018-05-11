{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Coordinate (Coordinate(..), Scope(..)) where

import           Data.Aeson.TH
import           Data.Char     (toLower)
import           Data.Text     (Text)
import qualified Data.Text     as Txt

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

data Scope = Compile | Provided | Runtime | System | Test
    deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions{ fieldLabelModifier = map toLower . take 2 . drop 1
                           , omitNothingFields = True
                           } ''Coordinate)

$(deriveJSON defaultOptions{ fieldLabelModifier = map toLower
                            , constructorTagModifier = \t -> [toLower (head t)]
                            } ''Scope)
