module Views.Tree exposing (viewTree)

import Data.Coordinate exposing (Coordinate)
import Data.Tree as Tree
import Html exposing (Html)
import Svg exposing (Svg, g, line, rect, text, text_)
import Svg.Attributes exposing (fill, height, stroke, textAnchor, transform, width, x1, x2, y1, y2)
import TreeDiagram exposing (defaultTreeLayout)
import TreeDiagram.Svg


convertTree : Tree.Tree Coordinate -> TreeDiagram.Tree Coordinate
convertTree (Tree.Node root children) =
    TreeDiagram.node root <| List.map convertTree children


viewTree : Tree.Tree Coordinate -> Html a
viewTree =
    convertTree
        >> TreeDiagram.Svg.draw
            { defaultTreeLayout
                | orientation = TreeDiagram.leftToRight
                , levelHeight = 300
                , siblingDistance = 50
                , subtreeDistance = 50
                , padding = 50
            }
            drawNode
            drawLine


{-| Represent edges as straight lines.
-}
drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    line
        [ x1 "0", y1 "0", x2 (toString targetX), y2 (toString targetY), stroke "black" ]
        []


{-| Represent nodes as white textboxes
-}
drawNode : Coordinate -> Svg msg
drawNode c =
    g
        []
        [ rect [ width "300", height "30", fill "salmon", transform "translate(-150,-15)" ] []
        , text_ [ width "300", textAnchor "middle" ] [ text <| c.artifactId ]
        ]
