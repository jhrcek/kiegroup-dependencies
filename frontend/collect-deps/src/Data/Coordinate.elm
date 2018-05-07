module Data.Coordinate exposing (Coordinate, decoder, toString)

import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias Coordinate =
    { groupId : String
    , artifactId : String
    , packaging : String
    , qualifier : Maybe String
    , version : String
    , isOur : Bool
    }


decoder : Decoder Coordinate
decoder =
    Decode.field "gr" string
        |> Decode.andThen
            (\groupId ->
                let
                    isOur =
                        isOurGroupId groupId
                in
                decode (\a p q v -> Coordinate groupId a p q v isOur)
                    |> required "ar" string
                    |> required "pa" string
                    |> optional "qu" (Decode.map Just string) Nothing
                    |> required "ve" string
            )


isOurGroupId : String -> Bool
isOurGroupId testedGroupId =
    List.any (\ourGroupId -> String.startsWith ourGroupId testedGroupId) ourGroupIds


ourGroupIds : List String
ourGroupIds =
    [ "org.kie"
    , "org.drools"
    , "org.jbpm"
    , "org.uberfire"
    , "org.dashbuilder"
    , "org.optaplanner"
    ]


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
