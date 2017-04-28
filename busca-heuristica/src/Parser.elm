module Parser exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (style)


type alias TileMap =
    Dict Position Tile


type alias Position =
    ( Int, Int )


type Tile
    = Cheese
    | Wall
    | Ground
    | Mouse
    | Exit


createMap : String -> Dict Position Tile
createMap content =
    let
        rows =
            String.lines content

        tilesChar =
            List.map (\row -> String.toList row) rows
    in
        List.indexedMap
            (\y row ->
                List.indexedMap (\x tileChar -> ( ( x, y ), toTile tileChar )) row
            )
            tilesChar
            |> List.concat
            |> Dict.fromList


toTile : Char -> Tile
toTile tileChar =
    case tileChar of
        '#' ->
            Wall

        'Q' ->
            Cheese

        '.' ->
            Ground

        'R' ->
            Mouse

        'S' ->
            Exit

        _ ->
            Wall


drawMap : TileMap -> Html msg
drawMap map =
    div
        [ style
            [ ( "display", "grid" ) ]
        ]
        (List.map (\( position, tile ) -> drawTile position tile) (Dict.toList map))


drawTile : Position -> Tile -> Html msg
drawTile ( x, y ) tile =
    let
        content =
            case tile of
                Wall ->
                    "wall"

                Cheese ->
                    "cheese"

                Ground ->
                    "ground"

                Mouse ->
                    "mouse"

                Exit ->
                    "exit"
    in
        div
            [ style
                [ ( "grid-column-start", toString x )
                , ( "grid-column-end", toString <| x + 1 )
                , ( "grid-row-start", toString y )
                , ( "grid-row-end", toString <| y + 1 )
                ]
            ]
            [ text content ]
