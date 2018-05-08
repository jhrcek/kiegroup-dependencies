module Page exposing (Page(CoordinateDetails, Home), parseLocation, toUrlHash)

import Graph exposing (NodeId)
import Navigation
import UrlParser as P exposing ((</>), Parser)


type Page
    = Home
    | CoordinateDetails NodeId


toUrlHash : Page -> String
toUrlHash page =
    let
        pieces =
            case page of
                Home ->
                    []

                CoordinateDetails x ->
                    [ "coordinate", toString x ]
    in
    String.join "/" ("#" :: pieces)


route : Parser (Page -> a) a
route =
    P.oneOf
        [ P.map Home P.top
        , P.map CoordinateDetails (P.s "coordinate" </> P.int)
        ]


parseLocation : Navigation.Location -> Page
parseLocation location =
    P.parseHash route location |> Maybe.withDefault Home
