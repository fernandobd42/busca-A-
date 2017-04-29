port module App exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, id, type_)
import Parser exposing (TileMap, createMap, drawMap)
import Sprites exposing (Sprites)


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
    , sprites : Sprites
    }


init : Sprites -> ( Model, Cmd Msg )
init { groundImg, wallImg, mouseImg, cheeseImg, doorImg } =
    ( { tileMap = Nothing
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


port sendFile : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendFile ->
            ( model, sendFile () )

        FileSended fileContent ->
            ( { model | tileMap = Just <| createMap fileContent }, Cmd.none )



-- SUBSCRIPTIONS


port fileSended : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    fileSended FileSended



-- VIEW


view : Model -> Html Msg
view model =
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
            [ input [ type_ "file", id "idFilePath", class "wrapper" ] []
            , button [ onClick SendFile, class "wrapper" ] [ text "Enviar" ]
            , h1 [] [ text "Labirinto" ]
            , drawComponent model.sprites model.tileMap
            ]
        ]


drawComponent : Sprites -> Maybe TileMap -> Html msg
drawComponent sprites tileMap =
    case tileMap of
        Just m ->
            drawMap sprites m

        Nothing ->
            div [] [ text "Envie o arquivo" ]
