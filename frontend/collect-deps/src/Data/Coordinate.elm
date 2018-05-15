module Data.Coordinate exposing (BackendCoordinate, Coordinate, decoder, highlight, toString)

import Html exposing (Attribute)
import Html.Attributes exposing (classList)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias ExtensibleCoordinate a =
    { a
        | groupId : String
        , artifactId : String
        , packaging : String
        , qualifier : Maybe String
        , version : String
    }


{-| From the backend we get just group:artifact:packaging:qualifier:version
-}
type alias BackendCoordinate =
    ExtensibleCoordinate {}


{-| In the frontend we calculate and cache additional info for speed

  - isOur - is it kiegroup or 3rd party dep?
  - transitiveDepsCount - number of transitive dependencies

-}
type alias Coordinate =
    ExtensibleCoordinate
        { isOur : Bool
        , transitiveDepsCount : Int
        , reverseTransitiveDepsCount : Int
        }


decoder : Decoder BackendCoordinate
decoder =
    decode
        (\g a p q v ->
            { groupId = g
            , artifactId = a
            , packaging = p
            , qualifier = q
            , version = v
            }
        )
        |> required "gr" string
        |> required "ar" string
        |> required "pa" string
        |> optional "qu" (Decode.map Just string) Nothing
        |> required "ve" string


toString : ExtensibleCoordinate a -> String
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
