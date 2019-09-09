module Color exposing (Color(..), Hue(..), Lightness(..), Rgb, fromRgb, toString)


type Color
    = Chromatic Lightness Hue
    | White
    | Black
    | Unknown Rgb


type Lightness
    = Light
    | Normal
    | Dark


type Hue
    = Red
    | Yellow
    | Green
    | Cyan
    | Blue
    | Magenta


type alias Rgb =
    { r : Int
    , g : Int
    , b : Int
    }


toString : Color -> String
toString color =
    case color of
        Chromatic lightness hue ->
            let
                ( hi, lo ) =
                    case lightness of
                        Light ->
                            ( "FF", "C0" )

                        Normal ->
                            ( "FF", "00" )

                        Dark ->
                            ( "C0", "00" )

                hex =
                    case hue of
                        Red ->
                            [ hi, lo, lo ]

                        Yellow ->
                            [ hi, hi, lo ]

                        Green ->
                            [ lo, hi, lo ]

                        Cyan ->
                            [ lo, hi, hi ]

                        Blue ->
                            [ lo, lo, hi ]

                        Magenta ->
                            [ hi, lo, hi ]
            in
            "#" ++ String.join "" hex

        White ->
            "#FFF"

        Black ->
            "#000"

        Unknown { r, g, b } ->
            "rgb(" ++ (String.join "," <| List.map String.fromInt [ r, g, b ]) ++ ")"


fromRgb : Rgb -> Color
fromRgb { r, g, b } =
    case ( r, g, b ) of
        ( 255, 255, 255 ) ->
            White

        ( 0, 0, 0 ) ->
            Black

        ( 255, 192, 192 ) ->
            Chromatic Light Red

        ( 255, 255, 192 ) ->
            Chromatic Light Yellow

        ( 192, 255, 192 ) ->
            Chromatic Light Green

        ( 192, 255, 255 ) ->
            Chromatic Light Cyan

        ( 192, 192, 255 ) ->
            Chromatic Light Blue

        ( 255, 192, 255 ) ->
            Chromatic Light Magenta

        ( 255, 0, 0 ) ->
            Chromatic Normal Red

        ( 255, 255, 0 ) ->
            Chromatic Normal Yellow

        ( 0, 255, 0 ) ->
            Chromatic Normal Green

        ( 0, 255, 255 ) ->
            Chromatic Normal Cyan

        ( 0, 0, 255 ) ->
            Chromatic Normal Blue

        ( 255, 0, 255 ) ->
            Chromatic Normal Magenta

        ( 192, 0, 0 ) ->
            Chromatic Dark Red

        ( 192, 192, 0 ) ->
            Chromatic Dark Yellow

        ( 0, 192, 0 ) ->
            Chromatic Dark Green

        ( 0, 192, 192 ) ->
            Chromatic Dark Cyan

        ( 0, 0, 192 ) ->
            Chromatic Dark Blue

        ( 192, 0, 192 ) ->
            Chromatic Dark Magenta

        ( _, _, _ ) ->
            Unknown (Rgb r g b)



-- vim: set ts=4 sw=4 et:
