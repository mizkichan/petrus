module Image exposing (Codel, ColorBlock, Image, decoder, empty, getColorBlocks)

import Color exposing (Color, Rgb)
import Json.Decode as D
import Point exposing (Point)


type Image
    = Image (List ColorBlock)


type alias ImageData =
    { width : Int
    , height : Int
    , data : List Int
    }


type alias Codel =
    { point : Point
    , rgb : Rgb
    }


type alias ColorBlock =
    { color : Color
    , area : Int
    , codels : List Point
    , table : Table
    }


type alias Table =
    { rl : Point
    , rr : Point
    , dl : Point
    , dr : Point
    , ll : Point
    , lr : Point
    , ul : Point
    , ur : Point
    }


getColorBlocks : Image -> List ColorBlock
getColorBlocks (Image cbs) =
    cbs


empty : Image
empty =
    Image []



-- DECODER


imageFromImageData : Int -> ImageData -> Image
imageFromImageData codelSize initialImageData =
    let
        helper : ImageData -> Int -> List Codel -> List Codel
        helper imageData i result =
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
                    in
                    helper
                        { imageData | data = List.drop skip rest }
                        (i + 1)
                        (Codel (Point x y) (Rgb r g b) :: result)

                _ ->
                    result
    in
    helper initialImageData 0 []
        |> colorBlocksFromCodels
        |> Image


colorBlocksFromCodels : List Codel -> List ColorBlock
colorBlocksFromCodels codels =
    codels
        |> Debug.todo "this won't work"
        |> List.gatherWith (\a b -> Point.distance a.point b.point <= 1 && a.rgb == b.rgb)
        |> List.map
            (\( repr, codels_ ) ->
                let
                    color =
                        Color.fromRgb repr.rgb

                    area =
                        List.length codels_

                    points =
                        List.map .point codels_

                    table =
                        generateTable points
                in
                ColorBlock color area points table
            )


generateTable : List Point -> Table
generateTable colorBlock =
    let
        ( xs, ys ) =
            ( List.map .x colorBlock
            , List.map .y colorBlock
            )

        ( xMin, xMax ) =
            ( List.minimum xs |> Maybe.withDefault 0
            , List.maximum xs |> Maybe.withDefault 0
            )

        ( yMin, yMax ) =
            ( List.minimum ys |> Maybe.withDefault 0
            , List.maximum ys |> Maybe.withDefault 0
            )

        rightEdge =
            List.filter (.x >> (==) xMax) colorBlock

        lowerEdge =
            List.filter (.y >> (==) yMax) colorBlock

        leftEdge =
            List.filter (.x >> (==) xMin) colorBlock

        upperEdge =
            List.filter (.y >> (==) yMin) colorBlock

        rl =
            rightEdge |> List.minimumBy .y |> Maybe.withDefault (Point 0 0)

        rr =
            rightEdge |> List.maximumBy .y |> Maybe.withDefault (Point 0 0)

        dl =
            lowerEdge |> List.maximumBy .x |> Maybe.withDefault (Point 0 0)

        dr =
            lowerEdge |> List.minimumBy .x |> Maybe.withDefault (Point 0 0)

        ll =
            leftEdge |> List.maximumBy .y |> Maybe.withDefault (Point 0 0)

        lr =
            leftEdge |> List.minimumBy .y |> Maybe.withDefault (Point 0 0)

        ul =
            upperEdge |> List.minimumBy .x |> Maybe.withDefault (Point 0 0)

        ur =
            upperEdge |> List.maximumBy .x |> Maybe.withDefault (Point 0 0)
    in
    Table rl rr dl dr ll lr ul ur


decoder : Int -> D.Decoder Image
decoder codelSize =
    D.map3 ImageData
        (D.field "width" D.int)
        (D.field "height" D.int)
        (D.field "data" (D.list D.int))
        |> D.map (imageFromImageData codelSize)



-- vim: set ts=4 sw=4 et:
