module Main exposing (main)

import Browser
import Element exposing (Attribute, Element, el, html, layout, none, text)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select exposing (file)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as D
import Json.Encode as E
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
    | ImageDecoded E.Value



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
    layout
        [ Font.family [ Font.monospace ]
        , padding
        ]
    <|
        column []
            [ row [ larger ]
                [ text "Petrus"
                , button { onPress = Just OpenButtonClicked, label = text "OPEN" }
                ]
            , model.error
                |> Maybe.map text
                |> Maybe.withDefault none
            , model.imageData
                |> Maybe.map (el [] << html << imageDataView)
                |> Maybe.withDefault none
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


row : List (Attribute msg) -> List (Element msg) -> Element msg
row a e =
    Element.row (spacing :: a) e


column : List (Attribute msg) -> List (Element msg) -> Element msg
column a e =
    Element.column (spacing :: a) e


button : { onPress : Maybe msg, label : Element msg } -> Element msg
button x =
    Input.button
        (List.append [ padding ] border)
        x


border : List (Attribute msg)
border =
    [ Border.solid, Border.width 1 ]


padding : Attribute msg
padding =
    Element.padding 8


spacing : Attribute msg
spacing =
    Element.spacing 8


larger : Attribute msg
larger =
    Element.htmlAttribute <|
        Html.Attributes.style "font-size" "larger"



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


decodeImageData : E.Value -> Result D.Error (Result String ImageData)
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
