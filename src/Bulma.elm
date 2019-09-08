module Bulma exposing (Modifier(..), box, button, buttons, column, columns, container, control, delete, field, fieldBody, fieldLabel, file, fileCta, fileIcon, fileLabel, fileName, has, hasBackground, icon, input, is, label, modal, modalBackground, modalClose, modalContent, navbar, navbarBrand, navbarEnd, navbarItem, navbarMenu, notification, section)

import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)


type alias E msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


builder : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Attribute msg) -> E msg
builder element attributes =
    element << List.append attributes



-- TYPES AND UTILITIES


type Modifier
    = White
    | Black
    | Light
    | Dark
    | Primary
    | Info
    | Link
    | Success
    | Warning
    | Danger
    | BlackBis
    | BlackTer
    | GreyDarker
    | GreyDark
    | Grey
    | GreyLight
    | GreyLighter
    | WhiteTer
    | WhiteBis
    | Active
    | Name
    | Hidden
    | Horizontal
    | Loading


modifierToString : Modifier -> String
modifierToString modifier =
    case modifier of
        White ->
            "white"

        Black ->
            "black"

        Light ->
            "light"

        Dark ->
            "dark"

        Primary ->
            "primary"

        Info ->
            "info"

        Link ->
            "link"

        Success ->
            "success"

        Warning ->
            "warning"

        Danger ->
            "danger"

        BlackBis ->
            "black-bis"

        BlackTer ->
            "black-ter"

        GreyDarker ->
            "grey-darker"

        GreyDark ->
            "grey-dark"

        Grey ->
            "grey"

        GreyLight ->
            "grey-light"

        GreyLighter ->
            "grey-lighter"

        WhiteTer ->
            "white-ter"

        WhiteBis ->
            "white-bis"

        Active ->
            "active"

        Name ->
            "name"

        Hidden ->
            "hidden"

        Horizontal ->
            "horizontal"

        Loading ->
            "loading"


is : Modifier -> String
is modifier =
    "is-" ++ modifierToString modifier


has : Modifier -> String
has modifier =
    "has-" ++ modifierToString modifier


hasBackground : Modifier -> String
hasBackground modifier =
    "has-background-" ++ modifierToString modifier



-- ELEMENTS


navbar : E msg
navbar =
    builder div [ class "navbar" ]


navbarBrand : E msg
navbarBrand =
    builder div [ class "navbar-brand" ]


navbarMenu : E msg
navbarMenu =
    builder div [ class "navbar-menu" ]


navbarEnd : E msg
navbarEnd =
    builder div [ class "navbar-end" ]


navbarItem :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
navbarItem func attributes elements =
    func
        (class "navbar-item" :: attributes)
        elements


button : E msg
button =
    builder div [ class "button" ]


notification : E msg
notification =
    builder div [ class "notification" ]


delete : E msg
delete =
    builder div [ class "delete" ]


container : E msg
container =
    builder div [ class "container" ]


columns : E msg
columns =
    builder div [ class "columns" ]


column : E msg
column =
    builder div [ class "column" ]


icon : E msg
icon =
    builder span [ class "icon" ]


box : E msg
box =
    builder div [ class "box" ]


section : E msg
section =
    builder Html.section [ class "section" ]


buttons : E msg
buttons =
    builder div [ class "buttons" ]


modal : E msg
modal =
    builder div [ class "modal" ]


modalBackground : E msg
modalBackground =
    builder div [ class "modal-background" ]


modalContent : E msg
modalContent =
    builder div [ class "modal-content" ]


modalClose : E msg
modalClose =
    builder Html.button [ class "modal-close" ]


file : E msg
file =
    builder div [ class "file" ]


fileLabel :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
fileLabel func attributes elements =
    func
        (class "file-label" :: attributes)
        elements


fileCta : E msg
fileCta =
    builder span [ class "file-cta" ]


fileIcon : E msg
fileIcon =
    builder span [ class "file-icon" ]


fileName : E msg
fileName =
    builder span [ class "file-name" ]


field : E msg
field =
    builder div [ class "field" ]


fieldLabel : E msg
fieldLabel =
    builder div [ class "field-label is-normal" ]


fieldBody : E msg
fieldBody =
    builder div [ class "field-body" ]


input : E msg
input =
    builder Html.input [ class "input" ]


label : E msg
label =
    builder Html.label [ class "label" ]


control : E msg
control =
    builder div [ class "control" ]



-- vim: set ts=4 sw=4 et:
