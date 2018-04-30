module Data.Tree exposing (Tree(Node), decoder)

import Json.Decode as Decode exposing (Decoder)


type Tree a
    = Node a (List (Tree a))


decoder : Decoder a -> Decoder (Tree a)
decoder itemDecoder =
    Decode.map2 Node
        (Decode.index 0 itemDecoder)
        (Decode.index 1 (Decode.list (Decode.lazy (\() -> decoder itemDecoder))))
