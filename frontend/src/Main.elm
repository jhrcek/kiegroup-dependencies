module Main exposing (main)

import Html exposing (Html, text)
import Html.Attributes exposing (href)
import Navigation
import Route


type Page
    = Home


main : Program Flags Model Msg
main =
    Navigation.programWithFlags
        UrlChanged
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = UrlChanged Navigation.Location


type alias Flags =
    List String


type alias Model =
    { kieArtifacts : List String
    , page : Page
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init artifactList location =
    ( setRoute (Route.fromLocation location)
        { kieArtifacts = List.sort artifactList
        , page = Home
        }
    , Cmd.none
    )


setRoute : Maybe Route.Route -> Model -> Model
setRoute mRoute model =
    let
        _ =
            Debug.log "Parsed Url " mRoute
    in
    model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.page of
        Home ->
            viewHome model.kieArtifacts


viewHome : Flags -> Html Msg
