module Image exposing (Codel, Image, decoder, empty, getCodels)

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


imageFromImageData : Int -> ImageData -> Image
imageFromImageData codelSize imageData =
    Image <| pixelsFromImageData imageData codelSize 0 []


pixelsFromImageData : ImageData -> Int -> Int -> List Pixel -> List Pixel
pixelsFromImageData imageData codelSize i result =
    case imageData.data of
        r :: g :: b :: _ :: rest ->
            let
                width =
                    imageData.width // codelSize

                x =
                    modBy width i

                y =
                    i // width

                skip =
                    if x == width - 1 then
                        (codelSize - 1) * (imageData.width + 1) * 4

                    else
                        (codelSize - 1) * 4

                next =
                    List.drop skip rest
            in
            pixelsFromImageData
                { imageData | data = next }
                codelSize
                (i + 1)
                (Pixel x y r g b :: result)

        _ ->
            result


decoder : Int -> D.Decoder Image
decoder codelSize =
    D.map3 ImageData
        (D.field "width" D.int)
        (D.field "height" D.int)
        (D.field "data" (D.list D.int))
        |> D.map (imageFromImageData codelSize)



-- vim: set ts=4 sw=4 et:
