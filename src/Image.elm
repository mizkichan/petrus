module Image exposing (Codel, Image, decoder, getCodels)

import Json.Decode as D


type Image
    = Image (List Pixel)


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



-- DECODER


intsToPixels : Int -> Int -> List Int -> List Pixel
intsToPixels width i list =
    case list of
        r :: g :: b :: _ :: rest ->
            Pixel (modBy width (i // 4)) ((i // 4) // width) r g b
                :: intsToPixels width (i + 4) rest

        _ ->
            []


widthHeightDecoder : D.Decoder ( Int, Int )
widthHeightDecoder =
    D.map2 Tuple.pair
        (D.field "width" D.int)
        (D.field "height" D.int)


makeDataDecoder : ( Int, Int ) -> D.Decoder Image
makeDataDecoder ( width, _ ) =
    D.field "data" (D.list D.int)
        |> D.map (Image << intsToPixels width 0)


decoder : D.Decoder (Result String Image)
decoder =
    D.oneOf
        [ D.string |> D.map Err
        , widthHeightDecoder |> D.andThen makeDataDecoder |> D.map Ok
        ]



-- vim: set ts=4 sw=4 et:
