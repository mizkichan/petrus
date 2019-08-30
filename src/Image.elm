module Image exposing (Codel, Image, decode, empty, getCodels)

import Json.Decode as D


type Image
    = Image (List Pixel)


type alias ImageData =
    { width : Int
    , height : Int
    , data : List Int
    }


type alias Pixel =
    { x : Int
    , y : Int
    , r : Int
    , g : Int
    , b : Int
    }


type alias Codel =
    {- TODO A codel is not a pixel. -}
    Pixel


getCodels : Image -> List Pixel
getCodels (Image codels) =
    codels


empty : Image
empty =
    Image []



-- DECODER


imageFromImageData : ImageData -> Image
imageFromImageData { width, height, data } =
    Image <| pixelsFromIntegers width 0 data []


pixelsFromIntegers : Int -> Int -> List Int -> List Pixel -> List Pixel
pixelsFromIntegers width i list result =
    case list of
        r :: g :: b :: _ :: rest ->
            pixelsFromIntegers width
                (i + 1)
                rest
                (Pixel (modBy width i) (i // width) r g b :: result)

        _ ->
            result


decoder : D.Decoder (Result String ImageData)
decoder =
    D.oneOf
        [ D.map Ok <|
            D.map3 ImageData
                (D.field "width" D.int)
                (D.field "height" D.int)
                (D.field "data" (D.list D.int))
        , D.map Err D.string
        ]


decode : D.Value -> Result String Image
decode =
    D.decodeValue
        decoder
        >> flattenResult D.errorToString
        >> Result.map imageFromImageData



-- MISC


flattenResult : (x -> y) -> Result x (Result y a) -> Result y a
flattenResult func =
    Result.mapError func >> Result.andThen identity



-- vim: set ts=4 sw=4 et:
