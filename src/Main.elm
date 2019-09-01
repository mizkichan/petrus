module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Bulma
import File exposing (File)
import File.Select exposing (file)
import Html exposing (Html, a, br, div, img, span, text)
import Html.Attributes exposing (href, id, src, target)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Image exposing (Image)
import Json.Decode as D
import Octicons
import Ports
import Svg exposing (Attribute, Svg, g, rect, svg)
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
    , mouseOperation : MouseOperation
    , scale : Float
    , offset : ( Float, Float )
    , viewBox : ViewBox
    }


type alias Notification =
    { color : Bulma.Color
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
      , mouseOperation = NoOp
      , scale = 1.0
      , offset = ( 0.0, 0.0 )
      , viewBox = ViewBox 0.0 0.0 0.0 0.0
      }
    , Dom.getElement "imageView" |> Task.attempt SetImageViewSize
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
                    [ Bulma.column []
                        [ imageView
                            { scale = model.scale
                            , offset = model.offset
                            , viewBox = model.viewBox
                            }
                            model.image
                        ]
                    , Bulma.column [] []
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


imageView : { scale : Float, offset : ( Float, Float ), viewBox : ViewBox } -> Image -> Svg Msg
imageView options image =
    Bulma.box []
        [ div [ id "imageView" ]
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


viewIf : Bool -> Html msg -> Html msg
viewIf condition html =
    if condition then
        html

    else
        text ""



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
