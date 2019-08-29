module Bulma exposing (button, column, columns, container, danger, delete, icon, large, navbar, navbarBrand, navbarEnd, navbarItemAnchor, navbarItemDiv, navbarMenu, navbarStart, notification, primary)

import Html exposing (Attribute, Html, a, div, i, nav, span)
import Html.Attributes exposing (class)
import Octicons


type alias E msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


builder : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Attribute msg) -> E msg
builder element attributes =
    element << List.append attributes



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


navbarStart : E msg
navbarStart =
    builder div [ class "navbar-start" ]


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


navbarItemAnchor : List (Attribute msg) -> List (Html msg) -> Html msg
navbarItemAnchor attributes elements =
    a
        (class "navbar-item" :: attributes)
        elements


navbarItemDiv : List (Attribute msg) -> List (Html msg) -> Html msg
navbarItemDiv attributes elements =
    div
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



-- ATTRIBUTES


danger : Attribute msg
danger =
    class "is-danger"


large : Attribute msg
large =
    class "is-large"


primary : Attribute msg
primary =
    class "is-primary"



-- vim: set ts=4 sw=4 et:
