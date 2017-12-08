module Main exposing (main)

import Html exposing (Html, text)
import Html.Attributes exposing (href)
import Navigation


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
    List String


init : List String -> Navigation.Location -> ( Model, Cmd Msg )
init artifactList location =
    ( List.sort artifactList, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [] <| List.map artifactLink model


artifactLink : String -> Html Msg
artifactLink gav =
    Html.div []
        [ Html.a [ href ("/" ++ gav ++ ".tgf") ] [ text gav ]
        ]
