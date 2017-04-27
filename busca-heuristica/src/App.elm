port module App exposing (..)

import Html exposing (..)
import Html.Events exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = SendFile
    | FileSended String


port sendFile : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendFile ->
            Debug.log "file" ( model, sendFile "" )

        FileSended fileContent ->
            let
                _ =
                    Debug.log "file sendend" fileContent
            in
                ( model, Cmd.none )



-- SUBSCRIPTIONS


port fileSended : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    fileSended FileSended



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendFile ] [ text "Enviar" ]
        ]
