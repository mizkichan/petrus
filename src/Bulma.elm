module Bulma exposing (box, button, buttons, column, columns, container, control, delete, field, fieldBody, fieldLabel, file, fileCta, fileIcon, fileLabel, fileName, hasBackgroundGrey, hasName, icon, input, isActive, isDanger, isHorizontal, isInfo, isLoading, isPrimary, isSuccess, isWarning, label, modal, modalBackground, modalClose, modalContent, navbar, navbarBrand, navbarEnd, navbarItem, navbarMenu, notification, section)

import Html exposing (Attribute, Html, div, span)
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



-- ATTRIBUTES


isActive : String
isActive =
    "is-active"


isHorizontal : String
isHorizontal =
    "is-horizontal"


isPrimary : String
isPrimary =
    "is-primary"


isSuccess : String
isSuccess =
    "is-success"


isInfo : String
isInfo =
    "is-info"


isWarning : String
isWarning =
    "is-warning"


isDanger : String
isDanger =
    "is-danger"


isLoading : String
isLoading =
    "is-loading"


hasName : String
hasName =
    "has-name"


hasBackgroundGrey : String
hasBackgroundGrey =
    "has-background-grey"



-- vim: set ts=4 sw=4 et:
