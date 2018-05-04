module Data.Scope exposing (Scope(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Scope
    = Compile
    | Provided
    | Runtime
    | System
    | Test


decoder : Decoder Scope
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "c" ->
                        Decode.succeed Compile

                    "p" ->
                        Decode.succeed Provided

                    "r" ->
                        Decode.succeed Runtime

                    "s" ->
                        Decode.succeed System

                    "t" ->
                        Decode.succeed Test

                    other ->
                        Decode.fail <| "Unrecognized scope " ++ other
            )
