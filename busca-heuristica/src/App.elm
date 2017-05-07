port module App exposing (..)

import Array
import Dict exposing (Dict)
import Time
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id, type_, style)
import TileMap exposing (TileMap, parseMap, Position, Tile(..))
import Sprites exposing (Sprites)
import Search


main : Program Sprites Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { tileMap : Maybe TileMap
    , path : Maybe Path
    , sprites : Sprites
    , index : Int
    }


type alias Path =
    List Position


init : Sprites -> ( Model, Cmd Msg )
init sprites =
    ( { tileMap = Nothing
      , path = Nothing
      , index = 0
      , sprites = sprites
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SendFile
    | FileSended String
    | Walk


port sendFile : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendFile ->
            model ! [ sendFile () ]

        FileSended fileContent ->
            let
                newTileMap =
                    parseMap fileContent
            in
                { model
                    | tileMap = Just newTileMap
                    , path = Search.findPath newTileMap
                    , index = 0
                }
                    ! []

        Walk ->
            case model.path of
                Just path ->
                    if model.index < (List.length path) - 1 then
                        { model | index = model.index + 1 } ! []
                    else
                        model ! []

                Nothing ->
                    model ! []



-- SUBSCRIPTIONS


port fileSended : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ fileSended FileSended, Time.every Time.second <| always Walk ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.tileMap of
        Just tileMap ->
            case model.path of
                Just mousePath ->
                    div [] <| drawTileMap model.sprites mousePath tileMap model.index

                Nothing ->
                    h1 [] [ text "Problema impossível" ]

        Nothing ->
            let
                styleWrapper =
                    [ ( "padding", "10px" )
                    , ( "background", "#293c4b" )
                    , ( "color", "white" )
                    , ( "border-radius", "100px" )
                    ]
            in
                div []
                    [ input [ type_ "file", id "idFilePath", style styleWrapper ] []
                    , button [ onClick SendFile, style styleWrapper ] [ text "Enviar" ]
                    ]


drawTileMap : Sprites -> Path -> TileMap -> Int -> List (Html msg)
drawTileMap sprites path tileMap index =
    let
        orNotFoundPosition : Maybe Position -> Position
        orNotFoundPosition =
            Maybe.withDefault ( -404, -404 )

        isFinishedWalk : Bool
        isFinishedWalk =
            index == (List.length path) - 1

        mouseSprite : ( Position, Tile )
        mouseSprite =
            if isFinishedWalk then
                ( Search.getFinalPosition tileMap |> orNotFoundPosition, OpenDoor )
            else
                ( Array.fromList path |> Array.get index |> orNotFoundPosition, Mouse )

        initialPosition : Position
        initialPosition =
            Search.getInitialPosition tileMap |> orNotFoundPosition

        entrySprite : ( Position, Tile )
        entrySprite =
            ( initialPosition, WalkedGround )

        walkedTiles : List Position
        walkedTiles =
            initialPosition :: (List.take index path)

        walkedSprites : List ( Position, Tile )
        walkedSprites =
            List.map (\pos -> ( pos, WalkedGround )) walkedTiles

        newTileMap : TileMap
        newTileMap =
            updateMap tileMap <| mouseSprite :: entrySprite :: walkedSprites
    in
        [ div
            [ style [ ( "width", "80%" ), ( "height", "100vh" ), ( "overflow", "auto" ), ( "position", "absolute" ) ] ]
            [ h1
                []
                [ text "Labirinto" ]
            , TileMap.drawMap sprites newTileMap
            ]
        , div
            [ style
                [ ( "width", "20%" )
                , ( "margin-left", "80%" )
                , ( "height", "100vh" )
                , ( "overflow", "auto" )
                , ( "position", "absolute" )
                ]
            ]
            [ div []
                [ text <|
                    (++)
                        "Número de queijos comidos: "
                        (walkedTiles
                            |> List.filter (\pos -> Dict.get pos tileMap == Just Cheese)
                            |> List.length
                            |> toString
                        )
                ]
            , div [] [ text <| "Número de passos: " ++ (toString <| List.length walkedTiles) ]
            , ul []
                (walkedTiles
                    |> List.indexedMap
                        (\stepNumber ( x, y ) ->
                            li [] [ text <| (toString <| stepNumber + 1) ++ " - (" ++ (toString y) ++ ", " ++ (toString x) ++ ")" ]
                        )
                )
            ]
        ]


updateMap : TileMap -> List ( Position, Tile ) -> TileMap
updateMap tileMap transforms =
    List.foldr
        (\( position, newTile ) currentTileMap ->
            Dict.update position (always <| Just newTile) currentTileMap
        )
        tileMap
        transforms
