module Data.Coordinate exposing (Coordinate, decoder)

import Json.Decode as Decode exposing (Decoder, field, string)


type alias Coordinate =
    { groupId : String
    , artifactId : String
    , packaging : String
    , version : String
    }


decoder : Decoder Coordinate
decoder =
    Decode.map4 Coordinate
        (field "grp" string)
        (field "art" string)
        (field "pkg" string)
        (field "ver" string)
