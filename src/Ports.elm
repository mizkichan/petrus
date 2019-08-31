port module Ports exposing (decodeImage, imageDecoded)

import Json.Decode as D


port decodeImage : String -> Cmd msg


port imageDecoded : (D.Value -> msg) -> Sub msg



-- vim: set ts=4 sw=4 et:
