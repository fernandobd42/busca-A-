port module App exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, id, type_, style)
import Parser exposing (TileMap, createMap, Position, Tile(..))
import Sprites exposing (Sprites)
import Search
import Dict
import Array
import Time exposing (every, second)


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
init { groundImg, wallImg, mouseImg, cheeseImg, doorImg } =
    ( { tileMap = Nothing
      , path = Nothing
      , index = 0
      , sprites =
            { wallImg = wallImg
            , groundImg = groundImg
            , mouseImg = mouseImg
            , cheeseImg = cheeseImg
            , doorImg = doorImg
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
    Sub.batch [ fileSended FileSended, every second Walk ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        content =
            (case model.tileMap of
                Just tileMap ->
                    case model.path of
                        Just mousePath ->
                            drawTileMap model.sprites mousePath tileMap model.index

                        Nothing ->
                            [ h1 [] [ text "Existe alguma inconsistência com este labirinto" ] ]

                Nothing ->
                    [ input [ type_ "file", id "idFilePath", class "wrapper" ] []
                    , button [ onClick SendFile, class "wrapper" ] [ text "Enviar" ]
                    ]
            )
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
        newTileMap =
            Dict.update
                (Array.fromList path
                    |> Array.get index
                    |> Maybe.withDefault ( 0, 0 )
                )
                (\_ -> Just Mouse)
                tileMap

        newTile =
            Dict.update
                (Maybe.withDefault ( 0, 0 ) (Search.getInitialPosition tileMap))
                (\_ -> Just Ground)
                newTileMap
    in
        [ h1 [] [ text "Labirinto" ]
        , Parser.drawMap sprites newTile
        ]
