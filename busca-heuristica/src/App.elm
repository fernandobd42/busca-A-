port module App exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id, type_)
import Parser exposing (TileMap, createMap, drawMap)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { tileMap : Maybe TileMap }


init : ( Model, Cmd Msg )
init =
    ( { tileMap = Nothing }, Cmd.none )



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
        [ input [ type_ "file", id "idFilePath" ] []
        , button [ onClick SendFile ] [ text "Enviar" ]
        , drawComponent model.tileMap
        ]


drawComponent : Maybe TileMap -> Html msg
drawComponent map =
    case map of
        Just m ->
            drawMap m

        Nothing ->
            div [] [ text "Envie o arquivo" ]
