module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onMouseMove)
import Bulma
import File exposing (File)
import File.Select exposing (file)
import Html exposing (Html, a, br, div, img, span, text)
import Html.Attributes exposing (href, src, target)
import Html.Events as Events exposing (onClick, onMouseDown, onMouseUp)
import Image exposing (Image)
import Json.Decode as D
import Octicons
import Ports
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes exposing (fill, height, transform, viewBox, width, x, y)
import Task



-- MAIN


main : Program D.Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- FLAGS


type alias Flags =
    { logoUrl : String
    , repositoryUrl : String
    , title : String
    }


defaultFlags : Flags
defaultFlags =
    Flags "" "" ""


decodeFlags : D.Value -> Result D.Error Flags
decodeFlags =
    D.decodeValue
        (D.map3 Flags
            (D.field "logoUrl" D.string)
            (D.field "repositoryUrl" D.string)
            (D.field "title" D.string)
        )



-- Model


type alias Model =
    { flags : Flags
    , notifications : List Notification
    , image : Image
    , isMouseDown : Bool
    , scale : Float
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
    | MouseDown
    | MouseUp
    | MouseMove ( Int, Int )
    | Wheel Float



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
      , isMouseDown = False
      , scale = 1.0
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Document Msg
view model =
    Document model.flags.title
        [ navbar
            { logoUrl = model.flags.logoUrl
            , repositoryUrl = model.flags.repositoryUrl
            }
        , notificationsView model.notifications
        , Bulma.section []
            [ Bulma.container []
                [ Bulma.columns []
                    [ Bulma.column [] [ imageView model.scale model.image ]
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


imageView : Float -> Image -> Svg Msg
imageView scale image =
    Bulma.box []
        [ svg
            [ width "100%"
            , height "100%"
            , viewBox "0 0 50 50"
            , onMouseDown MouseDown
            , onMouseUp MouseUp
            , onWheel Wheel
            ]
            [ g
                [ transformScale scale
                ]
                (image
                    |> Image.getCodels
                    |> List.map codelView
                )
            ]
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



-- ATTRIBUTES


transformScale : Float -> Svg.Attribute msg
transformScale scale =
    transform <| "scale(" ++ String.fromFloat scale ++ ")"



-- EVENTS


onWheel : (Float -> msg) -> Html.Attribute msg
onWheel msg =
    Events.preventDefaultOn "wheel" <|
        D.map
            (msg >> Tuple.pair >> (|>) True)
            (D.field "deltaY" D.float)



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
                D.decodeValue Image.decoder value
            of
                Ok image ->
                    ( { model | image = image }
                    , Cmd.none
                    )

                Err message ->
                    ( { model
                        | notifications =
                            Notification Bulma.Danger (D.errorToString message)
                                :: model.notifications
                      }
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

        MouseDown ->
            ( { model | isMouseDown = True }, Cmd.none )

        MouseUp ->
            ( { model | isMouseDown = False }, Cmd.none )

        MouseMove ( dx, dy ) ->
            {- TODO do something -}
            ( model, Cmd.none )

        Wheel delta ->
            let
                n =
                    if delta < 0 then
                        0.1

                    else
                        -0.1
            in
            ( { model | scale = model.scale * 2 ^ n }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        List.filterMap identity <|
            [ justIf model.isMouseDown (onMouseMove <| D.map MouseMove movementDecoder)
            , Just <| Ports.imageDecoded ImageDecoded
            ]


movementDecoder : D.Decoder ( Int, Int )
movementDecoder =
    D.map2 Tuple.pair
        (D.field "movementX" D.int)
        (D.field "movementY" D.int)



-- MISC


joinInt : String -> List Int -> String
joinInt separator =
    String.join separator
        << List.map String.fromInt


justIf : Bool -> a -> Maybe a
justIf condition value =
    if condition then
        Just value

    else
        Nothing



-- vim: set ts=4 sw=4 et:
