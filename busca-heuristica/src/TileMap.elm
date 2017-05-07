module TileMap exposing (TileMap, drawMap, parseMap, Position, Tile(..))

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (style)
import Sprites exposing (Sprites)


{-| Labirinto
-}
type alias TileMap =
    Dict Position Tile


type alias Position =
    ( Int, Int )


type Tile
    = Wall
    | Ground
    | Mouse
    | Cheese
    | Exit
    | WalkedGround
    | OpenDoor


{-| Converte o conteúdo do arquivo de texto em mapa
-}
parseMap : String -> TileMap
parseMap content =
    let
        rows : List String
        rows =
            String.lines content |> List.tail |> Maybe.withDefault []

        tilesChar : List (List Char)
        tilesChar =
            List.map (\row -> String.toList row) rows
    in
        List.indexedMap
            (\y row ->
                List.indexedMap (\x tileChar -> ( ( x, y ), charToTile tileChar )) row
            )
            tilesChar
            |> List.concat
            |> Dict.fromList


{-| Converte um caracter um uma imagem
-}
charToTile : Char -> Tile
charToTile tileChar =
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


{-| Converte um mapa em mapa
-}
drawMap : Sprites -> TileMap -> Html msg
drawMap sprites map =
    div
        [ style
            [ ( "display", "grid" )
            , ( "width", "  10vmin" )
            , ( "height", "10vmin" )
            , ( "position", "absolute" )
            , ( "margin", "auto" )
            ]
        ]
        (List.map (\( position, tile ) -> drawTile sprites position tile) <| Dict.toList map)


{-| Retorna uma div com a imagem da posição informada
-}
drawTile : Sprites -> Position -> Tile -> Html msg
drawTile sprites ( x, y ) tile =
    let
        sprite =
            case tile of
                Wall ->
                    sprites.wallImg

                Ground ->
                    sprites.groundImg

                Mouse ->
                    sprites.mouseImg

                Cheese ->
                    sprites.cheeseImg

                Exit ->
                    sprites.doorImg

                WalkedGround ->
                    sprites.walkedGround

                OpenDoor ->
                    sprites.openDoor
    in
        div
            [ style
                [ ( "grid-column-start", toString x )
                , ( "grid-column-end", toString <| x + 1 )
                , ( "grid-row-start", toString y )
                , ( "grid-row-end", toString <| y + 1 )
                , ( "background", "url(\"" ++ sprite ++ "\")" )
                , ( "background-size", "cover" )
                , ( "width", "50px" )
                , ( "height", "50px" )
                ]
            ]
            []
