module Image exposing (Codel, ColorBlock, Image, decoder, defs, empty, view)

import Bulma
import Color exposing (Color, Rgb)
import Json.Decode as D
import List.Extra as List
import Point exposing (Point)
import Svg exposing (Svg, g, rect, svg, use)
import Svg.Attributes exposing (class, fill, height, id, stroke, strokeWidth, viewBox, width, x, xlinkHref, y)


type alias Image =
    { width : Int
    , height : Int
    , colorBlocks : List ColorBlock
    }


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


type alias NonEmptyList a =
    ( a, List a )


empty : Image
empty =
    Image 0 0 []



-- DECODER


imageFromImageData : Int -> ImageData -> Image
imageFromImageData codelSize initialImageData =
    let
        helper : ImageData -> Int -> List Codel -> List Codel
        helper imageData i result =
            case imageData.data of
                r :: g :: b :: _ :: rest ->
                    let
                        w =
                            imageData.width // codelSize

                        x =
                            modBy w i

                        y =
                            i // w

                        skip =
                            if x == w - 1 then
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
        |> Image (initialImageData.width // codelSize) (initialImageData.height // codelSize)


colorBlocksFromCodels : List Codel -> List ColorBlock
colorBlocksFromCodels codels =
    let
        init : Codel -> NonEmptyList Codel
        init codel =
            ( codel, [] )

        helper : List (NonEmptyList Codel) -> List (NonEmptyList Codel)
        helper blocks =
            let
                gather : List (NonEmptyList Codel) -> Maybe (List (NonEmptyList (NonEmptyList Codel)))
                gather blocks_ =
                    let
                        gathered =
                            List.gatherWith gatherer blocks_
                    in
                    if List.length blocks_ /= List.length gathered then
                        Just gathered

                    else
                        Nothing

                gatherer : NonEmptyList Codel -> NonEmptyList Codel -> Bool
                gatherer ( x, xs ) ( y, ys ) =
                    let
                        hasSameColor =
                            case ( x.color, y.color ) of
                                ( Color.Chromatic xLightness xHue, Color.Chromatic yLightness yHue ) ->
                                    xLightness == yLightness && xHue == yHue

                                ( _, _ ) ->
                                    False

                        hasAdjacentCodel =
                            let
                                ps =
                                    List.map .point (x :: xs)

                                qs =
                                    List.map .point (y :: ys)
                            in
                            ps |> List.any (\p -> qs |> List.any (\q -> Point.distance p q == 1))
                    in
                    hasSameColor && hasAdjacentCodel

                merge : NonEmptyList (NonEmptyList Codel) -> NonEmptyList Codel
                merge ( ( x, xs ), xss ) =
                    let
                        tail =
                            List.concatMap toList xss

                        toList ( h, t ) =
                            h :: t
                    in
                    ( x, List.append xs tail )
            in
            blocks
                |> gather
                |> Maybe.map (List.map merge >> helper)
                |> Maybe.withDefault blocks

        finalize : NonEmptyList Codel -> ColorBlock
        finalize ( head, tail ) =
            let
                points =
                    List.map .point <| head :: tail
            in
            ColorBlock head.color points <| generateTable points
    in
    codels
        |> List.map init
        |> helper
        |> List.map finalize


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



-- VIEW


view : Image -> Svg msg
view image =
    Bulma.box []
        [ svg
            [ class <| Bulma.isBlock
            , viewBox <| "0 0 " ++ String.fromInt image.width ++ " " ++ String.fromInt image.height
            , width "100%"
            ]
            [ g [] (List.map colorBlockView image.colorBlocks) ]
        ]


colorBlockView : ColorBlock -> Svg msg
colorBlockView { codels, color } =
    g [ fill <| Color.toString color ] <| List.map codelView codels


codelView : Point -> Svg msg
codelView point =
    use
        [ xlinkHref "#codel"
        , x <| String.fromInt <| point.x
        , y <| String.fromInt <| point.y
        ]
        []


defs : Svg msg
defs =
    svg [ class Bulma.isHidden ]
        [ Svg.defs []
            [ rect
                [ id "codel"
                , width "1"
                , height "1"
                , stroke "black"
                , strokeWidth "0.01"
                ]
                []
            ]
        ]



-- vim: set ts=4 sw=4 et:
