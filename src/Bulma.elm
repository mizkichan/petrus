module Bulma exposing (Color(..), box, button, buttons, column, columns, container, delete, hasBackground, icon, is, navbar, navbarBrand, navbarEnd, navbarItem, navbarMenu, notification, section)

import Html exposing (Attribute, Html, div, nav, span)
import Html.Attributes exposing (class)


type alias E msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


builder : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Attribute msg) -> E msg
builder element attributes =
    element << List.append attributes



-- TYPES AND UTILITIES


type Color
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


colorToString : Color -> String
colorToString color =
    case color of
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


is : Color -> Attribute msg
is color =
    class <| "is-" ++ colorToString color


hasBackground : Color -> Attribute msg
hasBackground color =
    class <| "has-background-" ++ colorToString color



-- ELEMENTS


navbar : E msg
navbar =
    builder nav [ class "navbar" ]


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



-- vim: set ts=4 sw=4 et:
