module Utils exposing (cartesianMap, flip, uncurry, wrap)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


flip : (a -> b -> c) -> b -> a -> c
flip function argB argA =
    function argA argB


wrap min max n =
    if n < min then
        wrap min max <| n + (max - min)

    else if n >= max then
        wrap min max <| n - (max - min)

    else
        n


cartesianMap : (a -> b -> c) -> List a -> List b -> List (List c)
cartesianMap function rows columns =
    flip function
        |> flip List.map columns
        |> List.map (flip List.map rows)
