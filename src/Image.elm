module Image exposing (Codel, ColorBlock, Image, decoder, empty, getColorBlocks)

import Color exposing (Color, Rgb)
import Json.Decode as D
import List.Extra as List
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
    , color : Color
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
                        (Codel (Point x y) (Color.fromRgb <| Rgb r g b) :: result)

                _ ->
                    result
    in
    helper initialImageData 0 []
        |> colorBlocksFromCodels
        |> Image


colorBlocksFromCodels : List Codel -> List ColorBlock
colorBlocksFromCodels codels =
    let
        helper : ( Color, List Point ) -> ColorBlock
        helper ( color, points ) =
            ColorBlock color (List.length points) points (generateTable points)
    in
    codels
        |> partitionCodels
        |> List.map helper


partitionCodels : List Codel -> List ( Color, List Point )
partitionCodels codels =
    let
        gather : List Codel -> List ( Color, List Point ) -> List ( Color, List Point )
        gather codels_ result =
            case codels_ of
                head :: tail ->
                    let
                        nextResult =
                            if result |> List.any (isMemberOf head) then
                                result |> List.map (updatePair head)

                            else
                                newPair head.color head.point :: result
                    in
                    gather tail nextResult

                [] ->
                    result

        isMemberOf : Codel -> ( Color, List Point ) -> Bool
        isMemberOf codel ( color, points ) =
            (color == codel.color)
                && List.any ((==) 1 << Point.distance codel.point) points

        updatePair : Codel -> ( Color, List Point ) -> ( Color, List Point )
        updatePair codel ( color, points ) =
            if isMemberOf codel ( color, points ) then
                ( color, codel.point :: points )

            else
                ( color, points )

        newPair : Color -> Point -> ( Color, List Point )
        newPair color point =
            ( color, [ point ] )

        merge : List ( Color, List Point ) -> List ( Color, List Point )
        merge pairs =
            let
                hoge : ( ( Color, List Point ), List ( Color, List Point ) ) -> ( Color, List Point )
                hoge ( ( color, points ), pairs_ ) =
                    ( color, List.foldl fuga [] (( color, points ) :: pairs_) )

                fuga : ( Color, List Point ) -> List Point -> List Point
                fuga ( _, points ) result =
                    result ++ points
            in
            pairs
                |> List.gatherWith isToBeMergedWith
                |> List.map hoge

        isToBeMergedWith : ( Color, List Point ) -> ( Color, List Point ) -> Bool
        isToBeMergedWith ( colorA, pointsA ) ( colorB, pointsB ) =
            (colorA == colorB)
                && (product pointsA pointsB
                        |> List.any (\( pa, pb ) -> Point.distance pa pb == 1)
                   )

        product : List a -> List b -> List ( a, b )
        product xs ys =
            List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs
    in
    gather codels [] |> merge


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
