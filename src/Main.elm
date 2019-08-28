module Main exposing (main)

import Browser
import Bulma
import File exposing (File)
import File.Select exposing (file)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src)
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
    String



-- Model


type alias Model =
    { logoUrl : String
    , error : String
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
    ( { logoUrl = flags
      , error = ""
      , imageData = Nothing
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ navbar model.logoUrl
        , if not <| String.isEmpty model.error then
            Bulma.notification [ Bulma.danger ]
                [ text model.error
                , Bulma.delete [] []
                ]

          else
            text ""
        , model.imageData
            |> Maybe.map imageDataView
            |> Maybe.withDefault (text "")
        ]


navbar : String -> Html Msg
navbar logoUrl =
    Bulma.navbar []
        [ Bulma.navbarBrand []
            [ Bulma.navbarItem []
                [ img [ src logoUrl ] [] ]
            , Bulma.navbarItem []
                [ Bulma.button [ onClick OpenButtonClicked ]
                    [ text "Open" ]
                ]
            ]
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
                Ok imageData ->
                    ( { model | imageData = Just imageData, error = "" }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | error = error }
                    , Cmd.none
                    )


decodeImageData : D.Value -> Result String ImageData
decodeImageData =
    let
        splitIntoColors w i list =
            case list of
                r :: g :: b :: _ :: rest ->
                    Codel (modBy w i) (i // w) r g b :: splitIntoColors w (i + 4) rest

                _ ->
                    []

        widthHeightDecoder =
            D.map2 Tuple.pair
                (D.field "width" D.int)
                (D.field "height" D.int)

        makeDataDecoder ( w, h ) =
            D.field "data" (D.list D.int)
                |> D.map
                    (ImageData w h
                        << splitIntoColors w 0
                    )

        flattenResult =
            Result.andThen identity << Result.mapError D.errorToString
    in
    D.decodeValue
        (D.oneOf
            [ D.map Err D.string
            , widthHeightDecoder |> D.andThen makeDataDecoder |> D.map Ok
            ]
        )
        >> flattenResult



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
