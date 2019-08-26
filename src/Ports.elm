port module Ports exposing (..)

import Json.Encode as E


port decodeImage : String -> Cmd msg


port imageDecoded : (E.Value -> msg) -> Sub msg
