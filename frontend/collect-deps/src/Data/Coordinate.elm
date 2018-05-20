module Data.Coordinate
    exposing
        ( Coordinate
        , decoder
        , highlight
        , ourGroupIds
        , toString
        )

import Html exposing (Attribute)
import Html.Attributes exposing (classList)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias Coordinate =
    { groupId : String
    , artifactId : String
    , packaging : String
    , qualifier : Maybe String
    , version : String
    , isOur : Bool
    , directDepsCount : Int
    , transitiveDepsCount : Int
    , reverseDirectDepsCount : Int
    , reverseTransitiveDepsCount : Int
    }


decoder : Decoder Coordinate
decoder =
    Decode.map2 (,)
        (Decode.index 0 coordDecoder)
        (Decode.index 1 (Decode.list Decode.int))
        |> Decode.andThen
            (\( ( g, a, p, q, v ), counts ) ->
                case counts of
                    [ revTrans, rev, dir, trans ] ->
                        Decode.succeed
                            { groupId = g
                            , artifactId = a
                            , packaging = p
                            , qualifier = q
                            , version = v
                            , isOur = isOurGroupId g
                            , directDepsCount = dir
                            , transitiveDepsCount = trans
                            , reverseDirectDepsCount = rev
                            , reverseTransitiveDepsCount = revTrans
                            }

                    invalid ->
                        Decode.fail <| "Dependency counts contains unexpected fields: " ++ Basics.toString invalid
            )


coordDecoder : Decoder ( String, String, String, Maybe String, String )
coordDecoder =
    decode
        (\g a p q v -> ( g, a, p, q, v ))
        |> required "gr" string
        |> required "ar" string
        |> required "pa" string
        |> optional "qu" (Decode.map Just string) Nothing
        |> required "ve" string


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


highlight : Bool -> Attribute msg
highlight isHighlighted =
    classList [ ( "highlight", isHighlighted ) ]


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
