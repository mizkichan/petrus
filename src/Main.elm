module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Bulma
import Color exposing (Color)
import File exposing (File)
import File.Select exposing (file)
import Html exposing (Html, a, div, fieldset, label, span, text)
import Html.Attributes exposing (class, classList, disabled, href, id, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Image exposing (Image)
import Json.Decode as D
import Notification
import Octicons
import Point exposing (Point)
import Ports
import Svg exposing (Attribute, Svg, g, path, rect, svg)
import Svg.Attributes exposing (d, fill, height, transform, viewBox, width, x, y)
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
    , notifications : Notification.Manager
    , lastNotificationId : Int
    , image : Image
    , mouseOperation : MouseOperation
    , scale : Float
    , offset : ( Float, Float )
    , viewBox : ViewBox
    , modal : ModalModel
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


type alias ModalModel =
    { isActive : Bool
    , file : Maybe File
    , codelSize : Int
    , isDecoding : Bool
    }



-- MSG


type Msg
    = OpenFileDialog
    | FileSelected File
    | OpenFile File
    | UrlEncoded String
    | ImageDecoded D.Value
    | MouseDown Mouse.Event
    | MouseUp Mouse.Event
    | MouseMove Mouse.Event
    | Wheel Wheel.Event
    | SetImageViewSize (Result Dom.Error Dom.Element)
    | AddNotification Notification.Priority String
    | RemoveNotification Int
    | ActivateModal
    | DeactivateModal
    | SetCodelSize String



-- INIT


init : D.Value -> ( Model, Cmd Msg )
init flags =
    let
        ( decodedFlags, decodeError ) =
            case decodeFlags flags of
                Ok decoded ->
                    ( decoded, Nothing )

                Err error ->
                    ( defaultFlags, Just error )

        ( notifications, cmd ) =
            Notification.empty
                |> (decodeError
                        |> Maybe.map (D.errorToString >> Notification.add RemoveNotification Notification.Danger)
                        |> Maybe.withDefault (Notification.add RemoveNotification Notification.Info "Hello, world!")
                   )

        model =
            { flags = decodedFlags
            , notifications = notifications
            , lastNotificationId = 0
            , image = Image.empty
            , mouseOperation = NoOp
            , scale = 1.0
            , offset = ( 0.0, 0.0 )
            , viewBox = ViewBox 0.0 0.0 0.0 0.0
            , modal =
                { isActive = False
                , file = Nothing
                , codelSize = 1
                , isDecoding = False
                }
            }
    in
    ( model
    , Cmd.batch
        [ Dom.getElement "imageView" |> Task.attempt SetImageViewSize
        , cmd
        ]
    )



-- VIEW


view : Model -> Document Msg
view model =
    Document model.flags.title
        [ navbar { repositoryUrl = model.flags.repositoryUrl }
        , Notification.view RemoveNotification model.notifications
        , Bulma.section []
            [ Bulma.container []
                [ Bulma.columns []
                    [ Bulma.column []
                        [ Bulma.box []
                            [ Bulma.buttons []
                                [ Bulma.button [ onClick ActivateModal ] <| iconText Octicons.fileMedia "Open"
                                , Bulma.button [] <| iconText Octicons.circuitBoard "Build"
                                , Bulma.button [] <| iconText Octicons.rocket "Run"
                                ]
                            ]
                        , imageView
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
        , fileModalView model.modal
        ]


navbar : { repositoryUrl : String } -> Html Msg
navbar { repositoryUrl } =
    Bulma.navbar []
        [ Bulma.container []
            [ Bulma.navbarBrand [] [ Bulma.navbarItem div [] [ logo ] ]
            , Bulma.navbarMenu []
                [ Bulma.navbarEnd []
                    [ Bulma.navbarItem a
                        [ href "http://www.dangermouse.net/esoteric/piet.html"
                        , target "_blank"
                        ]
                      <|
                        textIcon Octicons.linkExternal "Piet language specification"
                    , Bulma.navbarItem a [ href repositoryUrl ] <|
                        iconText Octicons.markGithub "GitHub"
                    ]
                ]
            ]
        ]


logo : Svg msg
logo =
    svg [ height "24", viewBox "0 0 23 5" ] [ path [ d "M0,0v5h1v-2h1v-1h-1v-1h1v1h1v-2m1,0v5h3v-1h-2v-1h2v-1h-2v-1h2v-1m1,0v1h1v4h1v-4h1v-1m1,0v5h1v-2h1v2h1v-2h-1v-1h-1v-1h1v1h1v-2m1,0v5h3v-5h-1v4h-1v-4zm4,0v3h2v1h-2v1h3v-3h-2v-1h2v-1" ] [] ]


imageView : { scale : Float, offset : ( Float, Float ), viewBox : ViewBox } -> Image -> Svg Msg
imageView options image =
    Bulma.box []
        [ div [ id "imageView", class <| Bulma.hasBackgroundGrey ]
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
                        |> Image.getColorBlocks
                        |> List.map colorBlockView
                    )
                ]
            ]
        ]


colorBlockView : Image.ColorBlock -> Svg msg
colorBlockView { codels, color } =
    g [] <| List.map (codelView color) codels


codelView : Color -> Point -> Svg msg
codelView color point =
    rect
        [ x <| String.fromInt <| point.x
        , y <| String.fromInt <| point.y
        , width "1"
        , height "1"
        , fill <| Color.toString color
        ]
        []


fileModalView : ModalModel -> Html Msg
fileModalView options =
    Bulma.modal [ classList [ ( Bulma.isActive, options.isActive ) ] ]
        [ Bulma.modalBackground [ onClick DeactivateModal ] []
        , Bulma.modalContent []
            [ Bulma.box []
                [ fieldset [ disabled <| options.isDecoding ]
                    [ Bulma.field []
                        [ Bulma.file [ classList [ ( Bulma.hasName, options.file /= Nothing ) ] ]
                            [ Bulma.fileLabel label
                                []
                                [ Bulma.fileCta [ onClick OpenFileDialog ]
                                    [ Bulma.fileIcon [] [ Octicons.fileMedia Octicons.defaultOptions ]
                                    , Bulma.fileLabel span [] [ text "Choose a file..." ]
                                    ]
                                , options.file
                                    |> Maybe.map (File.name >> text >> List.singleton >> Bulma.fileName [])
                                    |> Maybe.withDefault (text "")
                                ]
                            ]
                        ]
                    , Bulma.field [ class <| Bulma.isHorizontal ]
                        [ Bulma.fieldLabel [] [ Bulma.label [] [ text "Codel Size" ] ]
                        , Bulma.fieldBody []
                            [ Bulma.field []
                                [ Bulma.control []
                                    [ Bulma.input
                                        [ type_ "number"
                                        , value <| String.fromInt options.codelSize
                                        , onInput <| SetCodelSize
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        ]
                    , Bulma.buttons []
                        [ Bulma.button
                            [ classList
                                [ ( Bulma.isPrimary, True )
                                , ( Bulma.isLoading, options.isDecoding )
                                ]
                            , options.file
                                |> Maybe.map (onClick << OpenFile)
                                |> Maybe.withDefault (disabled True)
                            ]
                            [ text "Open" ]
                        , Bulma.button [ onClick DeactivateModal ] [ text "Cancel" ]
                        ]
                    ]
                ]
            ]
        , Bulma.modalClose [ onClick DeactivateModal ] []
        ]


textIcon : (Octicons.Options -> Html msg) -> String -> List (Html msg)
textIcon icon string =
    [ span [] [ text string ]
    , Bulma.icon [] [ icon Octicons.defaultOptions ]
    ]


iconText : (Octicons.Options -> Html msg) -> String -> List (Html msg)
iconText icon string =
    [ Bulma.icon [] [ icon Octicons.defaultOptions ]
    , span [] [ text string ]
    ]



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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenFileDialog ->
            ( model, openFileDialog )

        FileSelected file ->
            ( { model | modal = setFile file model.modal }, Cmd.none )

        OpenFile file ->
            ( { model | modal = setDecoding model.modal }, Task.perform UrlEncoded <| File.toUrl file )

        UrlEncoded url ->
            ( model, Ports.decodeImage url )

        ImageDecoded value ->
            case D.decodeValue (Image.decoder model.modal.codelSize) value of
                Ok image ->
                    ( { model | image = image, modal = model.modal |> deactivateModal |> unsetDecoding }, Cmd.none )

                Err message ->
                    let
                        ( notifications, cmd ) =
                            Notification.add RemoveNotification Notification.Danger (D.errorToString message) model.notifications
                    in
                    ( { model | notifications = notifications }, cmd )

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
            case result of
                Ok value ->
                    let
                        size =
                            value.element.width
                    in
                    ( { model | viewBox = ViewBox 0.0 0.0 size size }, Cmd.none )

                Err (Dom.NotFound error) ->
                    let
                        ( notifications, cmd ) =
                            Notification.add RemoveNotification Notification.Danger error model.notifications
                    in
                    ( { model | notifications = notifications }, cmd )

        AddNotification priority message ->
            let
                ( notifications, cmd ) =
                    Notification.add RemoveNotification priority message model.notifications
            in
            ( { model | notifications = notifications }, cmd )

        RemoveNotification id ->
            ( { model | notifications = Notification.remove id model.notifications }, Cmd.none )

        ActivateModal ->
            ( { model | modal = activateModal model.modal }, openFileDialog )

        DeactivateModal ->
            ( { model | modal = deactivateModal model.modal }, Cmd.none )

        SetCodelSize value ->
            case String.toInt value of
                Just codelSize ->
                    ( { model | modal = setCodelSize codelSize model.modal }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


openFileDialog : Cmd Msg
openFileDialog =
    file [ "image/*" ] FileSelected


activateModal : ModalModel -> ModalModel
activateModal modal =
    { modal | isActive = True }


deactivateModal : ModalModel -> ModalModel
deactivateModal modal =
    { modal | isActive = False }


setFile : File -> ModalModel -> ModalModel
setFile file modal =
    { modal | file = Just file }


setDecoding : ModalModel -> ModalModel
setDecoding modal =
    { modal | isDecoding = True }


unsetDecoding : ModalModel -> ModalModel
unsetDecoding modal =
    { modal | isDecoding = False }


setCodelSize : Int -> ModalModel -> ModalModel
setCodelSize codelSize modal =
    { modal | codelSize = codelSize }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.imageDecoded ImageDecoded
        , Ports.error <| AddNotification Notification.Danger
        ]



-- MISC


mapJoin : (a -> String) -> String -> List a -> String
mapJoin func separator =
    String.join separator
        << List.map func



-- vim: set ts=4 sw=4 et:
