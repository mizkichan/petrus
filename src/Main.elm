module Main exposing (main)

import Browser
import Html exposing (Html, text)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- FLAGS


type alias Flags =
    ()



-- Model


type alias Model =
    ()



-- MSG


type alias Msg =
    ()



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( (), Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    text "Hello, world!"



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( (), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- vim: set ts=4 sw=4 et:
