module Main exposing (main)

import Browser
import Bulma
import File exposing (File)
import File.Select exposing (file)
import Html exposing (Html, a, br, div, img, span, text)
import Html.Attributes exposing (href, src, target)
import Html.Events exposing (onClick)
import Image exposing (Image)
import Json.Decode as D
import Octicons
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
    , notifications : List Notification
    , image : Image
    }


type alias Notification =
    { color : Bulma.Color
    , message : String
    }



-- MSG


type Msg
    = OpenButtonClicked
    | FileSelected File
    | UrlEncoded String
    | ImageDecoded D.Value
    | DeleteNotification Int



-- INIT


init : D.Value -> ( Model, Cmd Msg )
init flags =
    let
        ( decodedFlags, notifications ) =
            case decodeFlags flags of
                Ok decoded ->
                    ( decoded, [ Notification Bulma.Info "Hello, world!" ] )

                Err error ->
                    ( defaultFlags, [ Notification Bulma.Danger <| D.errorToString error ] )
    in
    ( { flags = decodedFlags
      , notifications = notifications
      , image = Image.empty
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ navbar
            { logoUrl = model.flags.logoUrl
            , repositoryUrl = model.flags.repositoryUrl
            }
        , notificationsView model.notifications
        , Bulma.section []
            [ Bulma.container []
                [ Bulma.columns []
                    [ Bulma.column [] [ imageView model.image ]
                    ]
                ]
            ]
        ]


navbar : { logoUrl : String, repositoryUrl : String } -> Html Msg
navbar { logoUrl, repositoryUrl } =
    Bulma.navbar []
        [ Bulma.container []
            [ Bulma.navbarBrand []
                [ Bulma.navbarItem div
                    []
                    [ img [ src logoUrl ] [] ]
                ]
            , Bulma.navbarMenu []
                [ Bulma.navbarStart []
                    [ Bulma.navbarItem div
                        []
                        [ Bulma.button [ onClick OpenButtonClicked ]
                            [ text "Open" ]
                        ]
                    ]
                , Bulma.navbarEnd []
                    [ Bulma.navbarItem a
                        [ href "http://www.dangermouse.net/esoteric/piet.html"
                        , target "_blank"
                        ]
                        [ span [] [ text "Piet language specification" ]
                        , Bulma.icon [] [ Octicons.linkExternal Octicons.defaultOptions ]
                        ]
                    , Bulma.navbarItem a
                        [ href repositoryUrl ]
                        [ Bulma.icon [] [ Octicons.markGithub Octicons.defaultOptions ]
                        , span [] [ text "GitHub" ]
                        ]
                    ]
                ]
            ]
        ]


notificationsView : List Notification -> Html Msg
notificationsView notifications =
    viewIf (not <| List.isEmpty notifications) <|
        Bulma.section []
            [ Bulma.container [] <|
                List.indexedMap
                    (\i notification ->
                        notification.message
                            |> String.lines
                            |> List.map String.trim
                            |> List.filter (not << String.isEmpty)
                            |> List.map text
                            |> List.intersperse (br [] [])
                            |> (::) (Bulma.delete [ onClick <| DeleteNotification i ] [])
                            |> Bulma.notification [ Bulma.fromColor notification.color ]
                    )
                    notifications
            ]


imageView : Image -> Svg msg
imageView image =
    Bulma.box []
        [ svg
            [ width "400"
            , height "400"
            , viewBox "0 0 50 50"
            ]
            (image
                |> Image.getCodels
                |> List.map codelView
            )
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
                    ( { model | image = image }
                    , Cmd.none
                    )

                Err message ->
                    ( { model | notifications = Notification Bulma.Danger message :: model.notifications }
                    , Cmd.none
                    )

        DeleteNotification i ->
            ( { model
                | notifications =
                    List.concat
                        [ List.take i model.notifications
                        , List.drop (i + 1) model.notifications
                        ]
              }
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
