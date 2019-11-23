port module Ports exposing (decodeImage, error, imageDecoded)

import Image exposing (ImageData)


port decodeImage : String -> Cmd msg


port imageDecoded : (ImageData -> msg) -> Sub msg


port error : (String -> msg) -> Sub msg



-- vim: set ts=4 sw=4 et:
