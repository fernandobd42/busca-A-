module Search exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import AStar
import TileMap exposing (TileMap, Position, Tile(..))


{-| Retorna a posição inicial do rato, se existir
-}
getInitialPosition : TileMap -> Maybe Position
getInitialPosition tileMap =
    Dict.foldr
        (\position tile found ->
            if tile == Mouse then
                Just position
            else
                found
        )
        Nothing
        tileMap


{-| Retorna a posição da saída do labirinto, se existir
-}
getFinalPosition : TileMap -> Maybe Position
getFinalPosition tileMap =
    Dict.foldr
        (\position tile found ->
            if tile == Exit then
                Just position
            else
                found
        )
        Nothing
        tileMap


{-| Retorna todas as posições andáveis em torna na posição informada
-}
getPossiblePaths : TileMap -> Position -> Set Position
getPossiblePaths tileMap ( currX, currY ) =
    let
        paths : List Position
        paths =
            [ ( currX + 1, currY )
            , ( currX - 1, currY )
            , ( currX, currY - 1 )
            , ( currX, currY + 1 )
            ]

        isWalkableTile : Position -> Bool
        isWalkableTile =
            isWalkable tileMap
    in
        List.foldr
            (\pos possiblePaths ->
                if isWalkableTile pos then
                    Set.insert pos possiblePaths
                else
                    possiblePaths
            )
            Set.empty
            paths


{-| Returna True se a posição informada é andável, False caso contrário
-}
isWalkable : TileMap -> Position -> Bool
isWalkable tileMap position =
    let
        tile : Tile
        tile =
            Dict.get position tileMap
                |> Maybe.withDefault Wall
    in
        tile /= Wall


{-| Retorna o caminho para a saida do labirinto, caso possível
-}
findPath : TileMap -> Maybe (List Position)
findPath tileMap =
    let
        notFoundPosition : Position
        notFoundPosition =
            ( -404, -404 )

        orNotFoundPosition : Maybe Position -> Position
        orNotFoundPosition =
            Maybe.withDefault notFoundPosition

        initialPosition : Position
        initialPosition =
            getInitialPosition tileMap |> orNotFoundPosition

        finalPosition : Position
        finalPosition =
            getFinalPosition tileMap |> orNotFoundPosition

        calcPossiblePath : Position -> Set Position
        calcPossiblePath =
            getPossiblePaths tileMap
    in
        if initialPosition == notFoundPosition || finalPosition == notFoundPosition then
            Nothing
        else
            AStar.findPath AStar.straightLineCost calcPossiblePath initialPosition finalPosition


{-| Retorna todas as posições que contém a imagem informada
-}
getTilesPosition : TileMap -> Tile -> Dict Position Tile
getTilesPosition tileMap wantedTile =
    tileMap
        |> Dict.toList
        |> List.filter (\( _, tile ) -> tile == wantedTile)
        |> Dict.fromList
