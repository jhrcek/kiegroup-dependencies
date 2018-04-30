module Data.Maven exposing (Coordinate, coordinateParser, coordinateToString)

import UrlParser


type Coordinate
    = Coordinate String


coordinateToString : Coordinate -> String
coordinateToString (Coordinate c) =
    c


coordinateParser : UrlParser.Parser (Coordinate -> a) a
coordinateParser =
    UrlParser.custom "COORDINATE" (Ok << Coordinate)
