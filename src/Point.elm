module Point exposing (Point, distance)


type alias Point =
    { x : Int
    , y : Int
    }


distance : Point -> Point -> Int
distance a b =
    abs (a.x - b.x) + abs (a.y - b.y)



-- vim: set ts=4 sw=4 et:
