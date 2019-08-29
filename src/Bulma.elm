module Bulma exposing (button, column, columns, container, danger, delete, navbar, navbarBrand, navbarEnd, navbarItem, navbarMenu, navbarStart, notification)

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)


type alias E msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


builder : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Attribute msg) -> E msg
builder element attributes =
    element << List.append attributes



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


navbarStart : E msg
navbarStart =
    builder div [ class "navbar-start" ]


navbarEnd : E msg
navbarEnd =
    builder div [ class "navbar-end" ]


navbarItem : E msg
navbarItem =
    builder div [ class "navbar-item" ]


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



-- ATTRIBUTES


danger : Attribute msg
danger =
    class "is-danger"



-- vim: set ts=4 sw=4 et:
