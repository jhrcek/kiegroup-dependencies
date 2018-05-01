module Data.Tree.Drawer exposing (Tree(Node), draw)


type Tree a
    = Node a (List (Tree a))


draw : Tree String -> String
draw =
    String.join "\n" << drawHelp


drawHelp : Tree String -> List String
drawHelp (Node root children) =
    let
        drawSubTrees ts =
            case ts of
                [] ->
                    []

                [ t ] ->
                    "│" :: shift "└─ " "   " (drawHelp t)

                t :: ts1 ->
                    "│" :: shift "├─ " "│  " (drawHelp t) ++ drawSubTrees ts1

        shift first other =
            List.map2 (++) (first :: List.repeat 20 other)
    in
    String.lines root ++ drawSubTrees children
