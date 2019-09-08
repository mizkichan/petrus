module Notification exposing (Manager, Notification, Priority(..), add, empty, remove, view)

import Bulma
import Html exposing (Html, br, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Process
import Task


type alias Manager =
    { lastNotificationId : Int
    , notifications : List Notification
    }


type alias Notification =
    { id : Int
    , priority : Priority
    , message : String
    }


type Priority
    = Success
    | Info
    | Warning
    | Danger


empty : Manager
empty =
    { lastNotificationId = 0
    , notifications = []
    }


add : (Int -> msg) -> Priority -> String -> Manager -> ( Manager, Cmd msg )
add msg priority message manager =
    let
        id =
            manager.lastNotificationId + 1

        cmd =
            case priority of
                Success ->
                    Process.sleep 10000 |> Task.perform (always <| msg id)

                Info ->
                    Process.sleep 30000 |> Task.perform (always <| msg id)

                Warning ->
                    Process.sleep 60000 |> Task.perform (always <| msg id)

                Danger ->
                    Cmd.none
    in
    ( { manager
        | lastNotificationId = id
        , notifications = Notification id priority message :: manager.notifications
      }
    , cmd
    )


remove : Int -> Manager -> Manager
remove id manager =
    { manager | notifications = List.filter (.id >> (/=) id) manager.notifications }


view : (Int -> msg) -> Manager -> Html msg
view onRemove manager =
    div [ class "notifications" ] <|
        List.map
            (\notification ->
                notification.message
                    |> String.lines
                    |> List.map String.trim
                    |> List.filter (not << String.isEmpty)
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> (::) (Bulma.delete [ onClick <| onRemove notification.id ] [])
                    |> Bulma.notification [ class <| priorityToClassName notification.priority ]
            )
            manager.notifications


priorityToClassName : Priority -> String
priorityToClassName priority =
    case priority of
        Success ->
            Bulma.isSuccess

        Info ->
            Bulma.isInfo

        Warning ->
            Bulma.isWarning

        Danger ->
            Bulma.isDanger



-- vim: set ts=4 sw=4 et:
