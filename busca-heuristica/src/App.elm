port module App exposing (..)

import Array
import Dict exposing (Dict)
import Time
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, id, type_, style)
import Parser exposing (TileMap, createMap, Position, Tile(..))
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
init { groundImg, wallImg, mouseImg, cheeseImg, doorImg, walkedGround } =
    ( { tileMap = Nothing
      , path = Nothing
      , index = 0
      , sprites =
            { wallImg = wallImg
            , groundImg = groundImg
            , mouseImg = mouseImg
            , cheeseImg = cheeseImg
            , doorImg = doorImg
            , walkedGround = walkedGround
            }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SendFile
    | FileSended String
    | Walk Time.Time


port sendFile : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendFile ->
            ( model, sendFile () )

        FileSended fileContent ->
            let
                newTileMap =
                    createMap fileContent
            in
                ( { model
                    | tileMap = Just <| newTileMap
                    , path = Search.findPath newTileMap
                    , index = 0
                  }
                , Cmd.none
                )

        Walk _ ->
            if model.path /= Nothing && model.index < (List.length <| Maybe.withDefault [] model.path) - 1 then
                { model | index = model.index + 1 } ! []
            else
                model ! []



-- SUBSCRIPTIONS


port fileSended : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ fileSended FileSended, Time.every Time.second Walk ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        content =
            case model.tileMap of
                Just tileMap ->
                    case model.path of
                        Just mousePath ->
                            drawTileMap model.sprites mousePath tileMap model.index

                        Nothing ->
                            [ h1 [] [ text "Problema impossível" ] ]

                Nothing ->
                    [ input [ type_ "file", id "idFilePath", class "wrapper" ] []
                    , button [ onClick SendFile, class "wrapper" ] [ text "Enviar" ]
                    ]
    in
        div []
            [ node "style"
                []
                [ text """

            body {
                background:rgba(0, 0, 0, 0.2);
            }

            .wrapper {
                padding: 10px;
                background: #293c4b;
                color: white;
                border-radius: 100px;
                }
         """ ]
            , div
                []
                content
            ]


drawTileMap : Sprites -> Path -> TileMap -> Int -> List (Html msg)
drawTileMap sprites path tileMap index =
    let
        mousePosition =
            ( Array.fromList path |> Array.get index, Mouse )

        initialPosition =
            ( Search.getInitialPosition tileMap, WalkedGround )

        ground =
            List.map (\( pos, tile ) -> ( pos, WalkedGround )) (getWalkedGround tileMap path index)

        cheeses =
            List.map (\( pos, tile ) -> ( pos, WalkedGround )) (getEatenCheese tileMap path index)

        removeInvalidTransforms : List ( Maybe Position, Tile ) -> List ( Position, Tile )
        removeInvalidTransforms transforms =
            (List.map
                (\( pos, tile ) -> ( Maybe.withDefault ( -1, -1 ) pos, tile ))
                transforms
            )
                |> List.filter (\( pos, _ ) -> pos /= ( -1, -1 ))

        newTileMap =
            updateMap tileMap <| List.append (removeInvalidTransforms [ mousePosition, initialPosition ]) (List.append ground cheeses)
    in
        [ div
            [ style [ ( "width", "80%" ), ( "height", "100vh" ), ( "overflow", "auto" ), ( "position", "absolute" ) ] ]
            [ h1
                []
                [ text "Labirinto" ]
            , Parser.drawMap sprites newTileMap
            ]
        , div
            [ style [ ( "width", "20%" ), ( "margin-left", "80%" ), ( "height", "100vh" ), ( "overflow", "auto" ), ( "position", "absolute" ) ] ]
            [ div [] [ text <| "Número de queijos comidos: " ++ (toString <| List.length cheeses) ]
            , div [] [ text <| "Número de passos: " ++ (toString index) ]
            , ul []
                (List.indexedMap
                    (\stepNumber position ->
                        li [] [ text <| (toString <| stepNumber + 1) ++ " - " ++ (toString position) ]
                    )
                 <|
                    List.take index path
                )
            ]
        ]


updateMap : TileMap -> List ( Position, Tile ) -> TileMap
updateMap tileMap transforms =
    List.foldr
        (\( position, newTile ) currentTileMap ->
            Dict.update position (\_ -> Just newTile) currentTileMap
        )
        tileMap
        transforms


getEatenCheese : TileMap -> Path -> Int -> List ( Position, Tile )
getEatenCheese tileMap path index =
    (Dict.intersect
        (Dict.fromList <|
            List.map (\pos -> ( pos, Cheese ))
                (List.take index path)
        )
        (Search.getTilesPosition tileMap Cheese)
    )
        |> Dict.toList


getWalkedGround : TileMap -> Path -> Int -> List ( Position, Tile )
getWalkedGround tileMap path index =
    (Dict.intersect
        (Dict.fromList <|
            List.map (\pos -> ( pos, Ground ))
                (List.take index path)
        )
        (Search.getTilesPosition tileMap Ground)
    )
        |> Dict.toList
