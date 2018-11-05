module Main exposing (Model, Msg(..), Pos, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Keyboard exposing (..)
import Keyboard.Arrows
import Maybe exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Pos =
    { x : Float
    , y : Float
    }


type alias Model =
    { position : Pos
    , velocity : Pos
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { position = { x = 0, y = 0 }, velocity = { x = 0, y = 0 } }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyDown RawKey
    | KeyUp RawKey
    | ApplyVelocity Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            ( keyDown key model, Cmd.none )

        KeyUp key ->
            ( keyUp key model, Cmd.none )

        ApplyVelocity time ->
            ( applyVelocity model, Cmd.none )


keyDown : RawKey -> Model -> Model
keyDown keyCode model =
    case withDefault ArrowDown (anyKey keyCode) of
        ArrowUp ->
            updateY -3 model

        ArrowLeft ->
            updateX -1.0 model

        ArrowRight ->
            updateX 1.0 model

        _ ->
            model


keyUp : RawKey -> Model -> Model
keyUp keyCode model =
    case withDefault ArrowDown (anyKey keyCode) of
        ArrowUp ->
            updateY -3 model

        ArrowLeft ->
            updateX 0 model

        ArrowRight ->
            updateX 0 model

        _ ->
            model


updateX : Float -> Model -> Model
updateX fl model =
    { model | velocity = { x = fl, y = model.velocity.y } }


updateY : Float -> Model -> Model
updateY fl model =
    if model.position.y == 115 then
        { model | velocity = { x = model.velocity.x, y = fl } }

    else
        model


applyVelocity : Model -> Model
applyVelocity model =
    if model.position.y + model.velocity.y > 115 then
        { position = { x = model.position.x + model.velocity.x, y = 115 }, velocity = { x = model.velocity.x, y = 0 } }

    else if model.position.y == 115 then
        { position = { x = model.position.x + model.velocity.x, y = model.position.y + model.velocity.y }, velocity = { x = model.velocity.x, y = model.velocity.y } }

    else
        { position = { x = model.position.x + model.velocity.x, y = model.position.y + model.velocity.y }, velocity = { x = model.velocity.x, y = model.velocity.y + 0.1 } }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Time.every 16.666666666666668 ApplyVelocity
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ viewBox "0 0 120 120"
            ]
            [ rect
                [ x (String.fromFloat model.position.x)
                , y (String.fromFloat model.position.y)
                , width "5"
                , height "5"
                ]
                []
            ]
        ]
