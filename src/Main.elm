module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Bulma
import File exposing (File)
import File.Select exposing (file)
import Html exposing (Html, a, br, div, span, text)
import Html.Attributes exposing (class, href, id, target)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Image exposing (Image)
import Json.Decode as D
import Octicons
import Ports
import Process
import Svg exposing (Attribute, Svg, g, path, rect, svg)
import Svg.Attributes exposing (d, fill, height, transform, viewBox, width, x, y)
import Task exposing (Task)



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
    { repositoryUrl : String
    , title : String
    }


defaultFlags : Flags
defaultFlags =
    Flags "" ""


decodeFlags : D.Value -> Result D.Error Flags
decodeFlags =
    D.decodeValue
        (D.map2 Flags
            (D.field "repositoryUrl" D.string)
            (D.field "title" D.string)
        )



-- Model


type alias Model =
    { flags : Flags
    , notifications : List Notification
    , lastNotificationId : Int
    , image : Image
    , mouseOperation : MouseOperation
    , scale : Float
    , offset : ( Float, Float )
    , viewBox : ViewBox
    }


type alias Notification =
    { id : Int
    , color : Bulma.Color
    , message : String
    }


type MouseOperation
    = NoOp
    | Drag
        { from : ( Float, Float )
        , to : ( Float, Float )
        }


type alias ViewBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }



-- MSG


type Msg
    = OpenButtonClicked
    | FileSelected File
    | UrlEncoded String
    | ImageDecoded D.Value
    | DeleteNotification Int
    | MouseDown Mouse.Event
    | MouseUp Mouse.Event
    | MouseMove Mouse.Event
    | Wheel Wheel.Event
    | SetImageViewSize (Result Dom.Error Dom.Element)
    | RemoveNotification Int



-- INIT


init : D.Value -> ( Model, Cmd Msg )
init flags =
    let
        hello =
            Notification 0 Bulma.Info "Hello, world!"

        ( decodedFlags, decodeError ) =
            case decodeFlags flags of
                Ok decoded ->
                    ( decoded, [] )

                Err error ->
                    ( defaultFlags, [ Notification 1 Bulma.Danger <| D.errorToString error ] )

        notifications =
            hello :: decodeError

        model =
            { flags = decodedFlags
            , notifications = notifications
            , lastNotificationId = 1
            , image = Image.empty
            , mouseOperation = NoOp
            , scale = 1.0
            , offset = ( 0.0, 0.0 )
            , viewBox = ViewBox 0.0 0.0 0.0 0.0
            }
    in
    ( model
    , Cmd.batch
        [ Dom.getElement "imageView" |> Task.attempt SetImageViewSize
        , Task.perform (always <| RemoveNotification 0) (Process.sleep 10000)
        ]
    )



-- VIEW


view : Model -> Document Msg
view model =
    Document model.flags.title
        [ navbar { repositoryUrl = model.flags.repositoryUrl }
        , notificationsView model.notifications
        , Bulma.section []
            [ Bulma.container []
                [ Bulma.columns []
                    [ Bulma.column []
                        [ imageView
                            { scale = model.scale
                            , offset = model.offset
                            , viewBox = model.viewBox
                            }
                            model.image
                        , Bulma.box []
                            [ Bulma.buttons []
                                [ Bulma.button [ onClick OpenButtonClicked ] [ text "Open" ]
                                , Bulma.button [] [ text "Compile" ]
                                , Bulma.button [] [ text "Run" ]
                                ]
                            ]
                        ]
                    , Bulma.column [] []
                    ]
                ]
            ]
        ]


navbar : { repositoryUrl : String } -> Html Msg
navbar { repositoryUrl } =
    Bulma.navbar []
        [ Bulma.container []
            [ Bulma.navbarBrand []
                [ Bulma.navbarItem div
                    []
                    [ logo ]
                ]
            , Bulma.navbarMenu []
                [ Bulma.navbarEnd []
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


logo : Svg msg
logo =
    svg [ height "24", viewBox "0 0 23 5" ] [ path [ d "M0,0v5h1v-2h1v-1h-1v-1h1v1h1v-2m1,0v5h3v-1h-2v-1h2v-1h-2v-1h2v-1m1,0v1h1v4h1v-4h1v-1m1,0v5h1v-2h1v2h1v-2h-1v-1h-1v-1h1v1h1v-2m1,0v5h3v-5h-1v4h-1v-4zm4,0v3h2v1h-2v1h3v-3h-2v-1h2v-1" ] [] ]


notificationsView : List Notification -> Html Msg
notificationsView notifications =
    div [ class "notifications" ] <|
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


imageView : { scale : Float, offset : ( Float, Float ), viewBox : ViewBox } -> Image -> Svg Msg
imageView options image =
    Bulma.box []
        [ div [ id "imageView", class "has-background-grey" ]
            [ svg
                [ width <| String.fromFloat options.viewBox.width
                , height <| String.fromFloat options.viewBox.height
                , viewBox <| mapJoin String.fromFloat " " <| [ 0.0, 0.0, options.viewBox.width, options.viewBox.height ]
                , Mouse.onDown MouseDown
                , Mouse.onUp MouseUp
                , Mouse.onMove MouseMove
                , Wheel.onWheel Wheel
                ]
                [ g
                    [ transforms
                        [ translate ( options.viewBox.width / 2, options.viewBox.height / 2 )
                        , scale options.scale
                        , translate ( -options.viewBox.width / 2, -options.viewBox.height / 2 )
                        , translate options.offset
                        ]
                    ]
                    (image
                        |> Image.getCodels
                        |> List.map codelView
                    )
                ]
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
            rgb ( codel.r, codel.g, codel.b )
        ]
        []



-- ATTRIBUTES


transforms : List String -> Attribute msg
transforms =
    transform << String.join " "


scale : Float -> String
scale value =
    "scale("
        ++ String.fromFloat value
        ++ ")"


translate : ( Float, Float ) -> String
translate ( x, y ) =
    "translate("
        ++ String.fromFloat x
        ++ " "
        ++ String.fromFloat y
        ++ ")"


rgb : ( Int, Int, Int ) -> String
rgb ( r, g, b ) =
    "rgb("
        ++ mapJoin String.fromInt "," [ r, g, b ]
        ++ ")"



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
                    model |> addNotification Bulma.Danger (D.errorToString message)

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

        MouseDown { clientPos } ->
            ( { model | mouseOperation = Drag { from = clientPos, to = clientPos } }, Cmd.none )

        MouseUp _ ->
            ( { model | mouseOperation = NoOp }, Cmd.none )

        MouseMove { clientPos } ->
            case model.mouseOperation of
                NoOp ->
                    ( model, Cmd.none )

                Drag { from, to } ->
                    let
                        ( xFrom, yFrom ) =
                            from

                        ( xTo, yTo ) =
                            clientPos

                        ( dx, dy ) =
                            ( xTo - xFrom, yTo - yFrom )
                    in
                    ( { model
                        | mouseOperation = Drag { from = to, to = clientPos }
                        , offset =
                            Tuple.mapBoth
                                ((+) (dx / model.scale / 2))
                                ((+) (dy / model.scale / 2))
                                model.offset
                      }
                    , Cmd.none
                    )

        Wheel { deltaY } ->
            let
                n =
                    if deltaY < 0 then
                        0.1

                    else
                        -0.1
            in
            ( { model | scale = model.scale * e ^ n }, Cmd.none )

        SetImageViewSize result ->
            let
                nextModel =
                    case result of
                        Ok { element } ->
                            {- This is not a typo, viewBox should be a shape of square. -}
                            { model | viewBox = ViewBox 0.0 0.0 element.width element.width }

                        Err _ ->
                            {- TODO handle error -}
                            model
            in
            ( nextModel, Cmd.none )

        RemoveNotification id ->
            ( { model | notifications = model.notifications |> List.filter (.id >> (/=) id) }, Cmd.none )


addNotification : Bulma.Color -> String -> Model -> ( Model, Cmd Msg )
addNotification color message model =
    let
        id =
            model.lastNotificationId + 1

        cmd =
            Task.perform (always <| RemoveNotification id) (Process.sleep 10000)
    in
    ( { model
        | lastNotificationId = id
        , notifications = Notification id color message :: model.notifications
      }
    , Task.perform (always <| RemoveNotification id) (Process.sleep 10000)
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.imageDecoded ImageDecoded



-- MISC


mapJoin : (a -> String) -> String -> List a -> String
mapJoin func separator =
    String.join separator
        << List.map func



-- vim: set ts=4 sw=4 et:
