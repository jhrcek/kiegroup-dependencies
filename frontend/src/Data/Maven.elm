module Data.Maven exposing (Coordinate, coordinateParser, coordinateToString)

import UrlParser


type Coordinate
    = Coordinate


coordinateToString : Coordinate -> String
coordinateToString =
    Debug.crash "TODO"


coordinateParser : UrlParser.Parser (Coordinate -> a) a
coordinateParser =
    Debug.crash "TODO"
