module Data.Scope exposing (Scope(..), decoder, toCssClass, toString)

import Json.Decode as Decode exposing (Decoder)


{-
   How many scope-edges are there of each type?
      Compile  10496
      Test     5209
      Provided 2578
      System   2129
      Runtime  945
-}


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


toString : Scope -> String
toString scope =
    case scope of
        Compile ->
            "compile"

        Provided ->
            "provided"

        Runtime ->
            "runtime"

        System ->
            "system"

        Test ->
            "test"


toCssClass : Scope -> String
toCssClass scope =
    "sc " ++ String.left 2 (toString scope)
