module Bulma exposing (button, danger, delete, navbar, navbarBrand, navbarItem, notification)

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)


type alias E msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


builder : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Attribute msg) -> E msg
builder element attributes =
    element << List.append attributes


navbar : E msg
navbar =
    builder div [ class "navbar" ]


navbarBrand : E msg
navbarBrand =
    builder div [ class "navbar-brand" ]


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


danger : Attribute msg
danger =
    class "is-danger"



-- vim: set ts=4 sw=4 et:
