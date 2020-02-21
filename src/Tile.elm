module Tile exposing
    ( Offset
    , Tile
    , fromLatLng
    , url
    , view
    )

import Html exposing (Html)
import Html.Attributes as Attr
import LatLng as LatLng exposing (LatLng, getLat, getLng)
import Regex exposing (replace)
import Utils exposing (uncurry, wrap)


type alias Url =
    String


type alias Tile =
    ( Url, Offset )


type alias Offset =
    { x : Float
    , y : Float
    }


url : String -> Int -> Int -> Int -> Url
url tileServer zoom x y =
    tileServer
        |> formatInt "{z}" zoom
        |> formatInt "{x}" x
        |> formatInt "{y}" y


fromLatLng : Float -> LatLng -> Offset
fromLatLng zoom loc =
    let
        n =
            2 ^ zoom

        x =
            n * ((getLng loc + 180) / 360) |> wrap 0 n

        latRad =
            getLat loc * pi / 180

        y =
            n * (1 - (logBase e <| abs <| tan latRad + (1 / cos latRad)) / pi) / 2
    in
    Offset x y


formatInt : String -> Int -> String -> String
formatInt userRegex number str =
    let
        userReplace : String -> (Regex.Match -> String) -> String -> String
        userReplace userRgx replacer string =
            case Regex.fromString userRgx of
                Nothing ->
                    string

                Just regex ->
                    Regex.replace regex replacer string
    in
    userReplace userRegex (\_ -> String.fromInt number) str


view : Float -> Tile -> Html msg
view tileSize ( tileUrl, offset ) =
    Html.img
        [ Attr.src tileUrl
        , Attr.style "position" "absolute"
        , Attr.style "left" <| String.fromFloat offset.x ++ "px"
        , Attr.style "top" <| String.fromFloat offset.y ++ "px"
        , Attr.style "width" <| String.fromFloat tileSize ++ "px"
        , Attr.style "height" <| String.fromFloat tileSize ++ "px"
        , Attr.style "background-color" <| "rgba(0,0,0, 0)"
        ]
        []
