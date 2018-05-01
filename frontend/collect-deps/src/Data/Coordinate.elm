module Data.Coordinate exposing (Coordinate, decoder, toString)

import Json.Decode as Decode exposing (Decoder, field, nullable, string)


type alias Coordinate =
    { groupId : String
    , artifactId : String
    , packaging : String
    , qualifier : Maybe String
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


toString : Coordinate -> String
toString c =
    let
        fields =
            case c.qualifier of
                Nothing ->
                    [ c.groupId, c.artifactId, c.packaging, c.version ]

                Just qualifier ->
                    [ c.groupId, c.artifactId, c.packaging, qualifier, c.version ]
    in
    String.join ":" fields
