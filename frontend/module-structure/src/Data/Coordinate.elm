module Data.Coordinate exposing (Coordinate, decoder)

import Json.Decode as Decode exposing (Decoder, field, nullable, string)


type alias Coordinate =
    { groupId : String
    , artifactId : String
    , packaging : String
    , qualified : Maybe String
    , version : String
    }


decoder : Decoder Coordinate
decoder =
    Decode.map5 Coordinate
        (field "grp" string)
        (field "art" string)
        (field "pkg" string)
        (field "qua" (nullable string))
        (field "ver" string)
