module Search exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import AStar
import Parser exposing (TileMap, Position, Tile(..))


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


getPossiblePaths : TileMap -> Position -> Set Position
getPossiblePaths tileMap currentPosition =
    let
        ( currX, currY ) =
            currentPosition

        rightPosition =
            ( currX + 1, currY )

        leftPosition =
            ( currX - 1, currY )

        upPosition =
            ( currX, currY - 1 )

        downPosition =
            ( currX, currY + 1 )

        possiblePaths =
            Set.empty

        isWalkableTile =
            isWalkable tileMap

        possiblePathsWithRight =
            if isWalkableTile rightPosition then
                Set.insert rightPosition possiblePaths
            else
                possiblePaths

        possiblePathsWithLeft =
            if isWalkableTile leftPosition then
                Set.insert leftPosition possiblePathsWithRight
            else
                possiblePathsWithRight

        possiblePathsWithUp =
            if isWalkableTile upPosition then
                Set.insert upPosition possiblePathsWithLeft
            else
                possiblePathsWithLeft

        possiblePathsWithDown =
            if isWalkableTile downPosition then
                Set.insert downPosition possiblePathsWithUp
            else
                possiblePathsWithUp
    in
        possiblePathsWithDown


isWalkable : TileMap -> Position -> Bool
isWalkable tileMap position =
    let
        tile =
            Dict.get position tileMap
                |> Maybe.withDefault Wall
    in
        tile /= Wall


findPath : TileMap -> Maybe (List Position)
findPath tileMap =
    let
        initialPosition =
            getInitialPosition tileMap

        finalPosition =
            getFinalPosition tileMap

        possiblePath =
            getPossiblePaths tileMap
    in
        if initialPosition == Nothing || finalPosition == Nothing then
            Nothing
        else
            AStar.findPath AStar.straightLineCost
                possiblePath
                (Maybe.withDefault ( 0, 0 ) initialPosition)
                (Maybe.withDefault ( 0, 0 ) finalPosition)


getTilesPosition : TileMap -> Tile -> Dict Position Tile
getTilesPosition tileMap wantedTile =
    (List.filter (\( position, tile ) -> tile == wantedTile) <|
        Dict.toList tileMap
    )
        |> Dict.fromList
