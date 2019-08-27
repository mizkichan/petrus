module Main exposing (main)

import Browser
import Bulma
import File exposing (File)
import File.Select exposing (file)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D
import Ports
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, viewBox, width, x, y)
import Task



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
    { error : Maybe String
    , imageData : Maybe ImageData
    }


type alias ImageData =
    { width : Int
    , height : Int
    , data : List Codel
    }


type alias Codel =
    { x : Int
    , y : Int
    , r : Int
    , g : Int
    , b : Int
    }



-- MSG


type Msg
    = OpenButtonClicked
    | FileSelected File
    | UrlEncoded String
    | ImageDecoded D.Value



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { error = Nothing
      , imageData = Nothing
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ navbar
        , model.error
            |> Maybe.withDefault ""
            |> text
        , model.imageData
            |> Maybe.map imageDataView
            |> Maybe.withDefault (text "")
        ]


navbar : Html Msg
navbar =
    Bulma.navbar []
        [ Bulma.navbarBrand []
            [ Bulma.navbarItem [] [ logo ]
            , Bulma.navbarItem []
                [ Bulma.button [ onClick OpenButtonClicked ]
                    [ text "Open" ]
                ]
            ]
        ]


logo : Svg msg
logo =
    svg [ width "120", viewBox "0 0 24 5" ]
        [ -- P
          rect [ x "0", y "0", width "1", height "5" ] []
        , rect [ x "1", y "0", width "1", height "1" ] []
        , rect [ x "1", y "2", width "1", height "1" ] []
        , rect [ x "2", y "0", width "1", height "2" ] []

        -- E
        , rect [ x "4", y "0", width "1", height "5" ] []
        , rect [ x "5", y "0", width "2", height "1" ] []
        , rect [ x "5", y "2", width "2", height "1" ] []
        , rect [ x "5", y "4", width "2", height "1" ] []

        -- T
        , rect [ x "8", y "0", width "3", height "1" ] []
        , rect [ x "9", y "1", width "1", height "4" ] []

        -- R
        , rect [ x "12", y "0", width "1", height "5" ] []
        , rect [ x "13", y "0", width "1", height "1" ] []
        , rect [ x "14", y "0", width "1", height "2" ] []
        , rect [ x "14", y "3", width "1", height "2" ] []
        , rect [ x "13", y "2", width "1", height "1" ] []

        -- U
        , rect [ x "16", y "0", width "1", height "5" ] []
        , rect [ x "17", y "4", width "1", height "1" ] []
        , rect [ x "18", y "0", width "1", height "5" ] []

        -- S
        , rect [ x "20", y "0", width "1", height "3" ] []
        , rect [ x "20", y "4", width "2", height "1" ] []
        , rect [ x "21", y "0", width "2", height "1" ] []
        , rect [ x "21", y "2", width "1", height "1" ] []
        , rect [ x "22", y "2", width "1", height "3" ] []
        ]


imageDataView : ImageData -> Svg msg
imageDataView imageData =
    imageData.data
        |> List.map codelView
        |> svg
            [ width "400"
            , height "400"
            , viewBox <| joinInt "," [ 0, 0, imageData.width, imageData.height ]
            ]


codelView : Codel -> Svg msg
codelView codel =
    rect
        [ x <| String.fromInt <| codel.x
        , y <| String.fromInt <| codel.y
        , width "1"
        , height "1"
        , fill <|
            "rgb("
                ++ joinInt "," [ codel.r, codel.g, codel.b ]
                ++ ")"
        ]
        []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenButtonClicked ->
            ( model, file [ "image/*" ] FileSelected )

        FileSelected file ->
            ( model, Task.perform UrlEncoded <| File.toUrl file )

        UrlEncoded url ->
            ( model, Ports.decodeImage url )

        ImageDecoded value ->
            case
                decodeImageData value
            of
                Ok (Ok imageData) ->
                    ( { model | imageData = Just imageData }
                    , Cmd.none
                    )

                Ok (Err err) ->
                    ( { model | error = Just err }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | error = Just <| D.errorToString err }
                    , Cmd.none
                    )


decodeImageData : D.Value -> Result D.Error (Result String ImageData)
decodeImageData =
    D.oneOf
        [ D.map Err D.string
        , D.map Ok <|
            D.map3 ImageData
                (D.field "width" D.int)
                (D.field "height" D.int)
                (D.field "data" <|
                    D.list <|
                        D.map5
                            Codel
                            (D.field "x" D.int)
                            (D.field "y" D.int)
                            (D.field "r" D.int)
                            (D.field "g" D.int)
                            (D.field "b" D.int)
                )
        ]
        |> D.decodeValue



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.imageDecoded ImageDecoded



-- MISC


joinInt : String -> List Int -> String
joinInt separator =
    String.join separator
        << List.map String.fromInt



-- vim: set ts=4 sw=4 et:
