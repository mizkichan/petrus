module Main exposing (main)

import Browser exposing (Document)
import Bulma
import File exposing (File)
import File.Select exposing (file)
import Html exposing (Html, a, div, fieldset, label, span, text)
import Html.Attributes exposing (class, classList, disabled, href, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Image exposing (Image)
import Json.Decode as D
import Octicons
import Ports
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, height)
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { notification : String
    , lastNotificationId : Int
    , image : Image
    , modal : ModalModel
    , isNavbarActive : Bool
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
    | ActivateModal
    | DeactivateModal
    | SetCodelSize String
    | ToggleNavbar
    | ShowNotification String



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { notification = ""
      , lastNotificationId = 0
      , image = Image.empty
      , modal =
            { isActive = False
            , file = Nothing
            , codelSize = 1
            , isDecoding = False
            }
      , isNavbarActive = False
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Document Msg
view model =
    Document "Petrus - A Piet compiler."
        [ Image.defs
        , navbar model.isNavbarActive
        , Bulma.section []
            [ Bulma.container []
                [ notification model.notification
                , Bulma.columns []
                    [ Bulma.column []
                        [ Bulma.box []
                            [ Bulma.buttons []
                                [ Bulma.button [ onClick ActivateModal ] [ iconText Octicons.fileMedia "Open" ]
                                , Bulma.button [] [ iconText Octicons.circuitBoard "Build" ]
                                , Bulma.button [] [ iconText Octicons.rocket "Run" ]
                                ]
                            ]
                        , Image.view model.image
                        ]
                    , Bulma.column [] []
                    ]
                ]
            ]
        , fileModalView model.modal
        ]


navbar : Bool -> Html Msg
navbar isActive =
    Bulma.navbar []
        [ Bulma.container []
            [ Bulma.navbarBrand []
                [ Bulma.navbarItem div [] [ logo ]
                , Bulma.navbarBurger
                    [ classList [ ( Bulma.isActive, isActive ) ]
                    , onClick ToggleNavbar
                    ]
                ]
            , Bulma.navbarMenu [ classList [ ( Bulma.isActive, isActive ) ] ]
                [ Bulma.navbarEnd []
                    [ Bulma.navbarItem a
                        [ href "http://www.dangermouse.net/esoteric/piet.html", target "_blank" ]
                        [ textIcon Octicons.linkExternal "Piet language specification" ]
                    , Bulma.navbarItem a
                        [ href "https://github.com/mizkichan/petrus" ]
                        [ iconText Octicons.markGithub "GitHub" ]
                    ]
                ]
            ]
        ]


logo : Html msg
logo =
    svg [ height "24", viewBox [ 0, 0, 23, 5 ] ] [ path [ d "M0,0v5h1v-2h1v-1h-1v-1h1v1h1v-2m1,0v5h3v-1h-2v-1h2v-1h-2v-1h2v-1m1,0v1h1v4h1v-4h1v-1m1,0v5h1v-2h1v2h1v-2h-1v-1h-1v-1h1v1h1v-2m1,0v5h3v-5h-1v4h-1v-4zm4,0v3h2v1h-2v1h3v-3h-2v-1h2v-1" ] [] ]


notification : String -> Html Msg
notification message =
    if String.isEmpty message then
        text ""

    else
        Bulma.notification []
            [ Bulma.delete [ onClick <| ShowNotification "" ] []
            , text message
            ]


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


textIcon : (Octicons.Options -> Html msg) -> String -> Html msg
textIcon icon string =
    span []
        [ span [] [ text string ]
        , Bulma.icon [] [ icon Octicons.defaultOptions ]
        ]


iconText : (Octicons.Options -> Html msg) -> String -> Html msg
iconText icon string =
    span []
        [ Bulma.icon [] [ icon Octicons.defaultOptions ]
        , span [] [ text string ]
        ]



-- ATTRIBUTES


viewBox : List Float -> Svg.Attribute msg
viewBox vb =
    Svg.Attributes.viewBox <| String.join " " <| List.map String.fromFloat vb



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
                    ( { model
                        | image = image
                        , modal = model.modal |> deactivateModal |> unsetDecoding
                      }
                    , Cmd.none
                    )

                Err message ->
                    ( { model | notification = D.errorToString message }, Cmd.none )

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

        ToggleNavbar ->
            ( { model | isNavbarActive = not model.isNavbarActive }, Cmd.none )

        ShowNotification message ->
            ( { model | notification = message }, Cmd.none )


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
subscriptions _ =
    Sub.batch
        [ Ports.imageDecoded ImageDecoded
        , Ports.error ShowNotification
        ]



-- vim: set ts=4 sw=4 et:
