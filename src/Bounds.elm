module Bounds exposing
    ( Bounds
    , Zoom
    , findCenter
    , zoom
    )

import LatLng as LatLng exposing (LatLng, getLat, getLng, latLng)
import Tile exposing (Offset)
import Utils exposing (wrap)


type alias Bounds a =
    { northEast : a
    , southWest : a
    }


type alias Zoom =
    Float


zoom : Float -> Float -> Float -> Bounds LatLng -> Zoom
zoom tileSize mapWidth mapHeight bounds =
    let
        latY lat =
            sin (lat * pi / 180)

        ( ne, sw ) =
            ( bounds.northEast, bounds.southWest )

        radX2 lat =
            logBase e ((1 + latY lat) / (1 - latY lat)) / 2

        latRad lat =
            (max -pi <| min (radX2 lat) pi) / 2

        latFraction =
            latRad (getLat ne) - latRad (getLat sw)

        lngFraction =
            ((getLng ne - getLng sw) |> wrap 0 360) / 360

        zoomFn mapSize tileSize2 frac =
            logBase 2 (mapSize / tileSize2 / frac)
    in
    min
        (zoomFn mapWidth tileSize lngFraction)
        (zoomFn mapHeight tileSize latFraction)


findCenter : Bounds LatLng -> LatLng
findCenter bounds =
    latLng
        { lat = getLat bounds.northEast - getLat bounds.southWest / 2
        , lng = getLng bounds.northEast - getLng bounds.southWest / 2
        }
