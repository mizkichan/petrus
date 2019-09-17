module Bulma exposing (box, button, buttons, column, columns, container, control, delete, field, fieldBody, fieldLabel, file, fileCta, fileIcon, fileLabel, fileName, hasName, icon, input, isActive, isBlock, isDanger, isHorizontal, isInfo, isLoading, isPrimary, isSuccess, isWarning, label, modal, modalBackground, modalClose, modalContent, navbar, navbarBrand, navbarEnd, navbarItem, navbarMenu, notification, section)

import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)


type alias E msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


builder : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> E msg
builder element className =
    element << List.append [ class className ]



-- ELEMENTS


navbar : E msg
navbar =
    builder div "navbar"


navbarBrand : E msg
navbarBrand =
    builder div "navbar-brand"


navbarMenu : E msg
navbarMenu =
    builder div "navbar-menu"


navbarEnd : E msg
navbarEnd =
    builder div "navbar-end"


navbarItem : E msg -> E msg
navbarItem func =
    builder func "navbar-item"


button : E msg
button =
    builder Html.button "button"


notification : E msg
notification =
    builder div "notification"


delete : E msg
delete =
    builder div "delete"


container : E msg
container =
    builder div "container"


columns : E msg
columns =
    builder div "columns"


column : E msg
column =
    builder div "column"


icon : E msg
icon =
    builder span "icon"


box : E msg
box =
    builder div "box"


section : E msg
section =
    builder Html.section "section"


buttons : E msg
buttons =
    builder div "buttons"


modal : E msg
modal =
    builder div "modal"


modalBackground : E msg
modalBackground =
    builder div "modal-background"


modalContent : E msg
modalContent =
    builder div "modal-content"


modalClose : E msg
modalClose =
    builder Html.button "modal-close"


file : E msg
file =
    builder div "file"


fileLabel : E msg -> E msg
fileLabel func =
    builder func "file-label"


fileCta : E msg
fileCta =
    builder span "file-cta"


fileIcon : E msg
fileIcon =
    builder span "file-icon"


fileName : E msg
fileName =
    builder span "file-name"


field : E msg
field =
    builder div "field"


fieldLabel : E msg
fieldLabel =
    builder div "field-label is-normal"


fieldBody : E msg
fieldBody =
    builder div "field-body"


input : E msg
input =
    builder Html.input "input"


label : E msg
label =
    builder Html.label "label"


control : E msg
control =
    builder div "control"



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


isBlock : String
isBlock =
    "is-block"


hasName : String
hasName =
    "has-name"



-- vim: set ts=4 sw=4 et:
