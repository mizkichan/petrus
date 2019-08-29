module Main exposing (main)

import Browser
import Bulma
import File exposing (File)
import File.Select exposing (file)
import Html exposing (Html, a, div, img, text)
import Html.Attributes exposing (href, src)
import Html.Events exposing (onClick)
import Image exposing (Image)
import Json.Decode as D
import Ports
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, viewBox, width, x, y)
import Task



-- MAIN


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- FLAGS


type alias Flags =
    { logoUrl : String
    , repositoryUrl : String
    }


defaultFlags : Flags
defaultFlags =
    Flags "" ""


decodeFlags : D.Value -> Result D.Error Flags
decodeFlags =
    D.decodeValue
        (D.map2 Flags
            (D.field "logoUrl" D.string)
            (D.field "repositoryUrl" D.string)
        )



-- Model


type alias Model =
    { flags : Flags
    , error : String
    , image : Maybe Image
    }



-- MSG


type Msg
    = OpenButtonClicked
    | FileSelected File
    | UrlEncoded String
    | ImageDecoded D.Value



-- INIT


init : D.Value -> ( Model, Cmd Msg )
init flags =
    let
        ( decodedFlags, decodeError ) =
            case decodeFlags flags of
                Ok decoded ->
                    ( decoded, "" )

                Err error ->
                    ( defaultFlags, D.errorToString error )
    in
    ( { flags = decodedFlags
      , error = decodeError
      , image = Nothing
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    Bulma.container []
        [ navbar
            { logoUrl = model.flags.logoUrl
            , repositoryUrl = model.flags.repositoryUrl
            }
        , viewIf (not <| String.isEmpty model.error) <|
            Bulma.notification [ Bulma.danger ]
                [ text model.error
                , Bulma.delete [] []
                ]
        , Bulma.columns []
            [ Bulma.column []
                [ model.image
                    |> Maybe.map imageView
                    |> Maybe.withDefault (text "")
                ]
            ]
        ]


navbar : { logoUrl : String, repositoryUrl : String } -> Html Msg
navbar { logoUrl, repositoryUrl } =
    Bulma.navbar []
        [ Bulma.navbarBrand []
            [ Bulma.navbarItem []
                [ img [ src logoUrl ] [] ]
            ]
        , Bulma.navbarMenu []
            [ Bulma.navbarStart []
                [ Bulma.navbarItem []
                    [ Bulma.button [ onClick OpenButtonClicked ]
                        [ text "Open" ]
                    ]
                ]
            , Bulma.navbarEnd []
                [ Bulma.navbarItem []
                    [ a [ href repositoryUrl ]
                        [ text "GitHub" ]
                    ]
                ]
            ]
        ]


imageView : Image -> Svg msg
imageView image =
    Image.getCodels image
        |> List.map codelView
        |> svg
            [ width "400"
            , height "400"
            , viewBox "0 0 50 50"
            ]


codelView : Image.Codel -> Svg msg
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


viewIf : Bool -> Html msg -> Html msg
viewIf condition html =
    if condition then
        html

    else
        text ""



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
                Image.decode value
            of
                Ok image ->
                    ( { model | image = Just image, error = "" }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | error = error }
                    , Cmd.none
                    )



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
