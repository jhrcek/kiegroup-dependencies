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
    Decode.oneOf []
