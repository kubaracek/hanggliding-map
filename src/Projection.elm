module Projection exposing (..)

import Bounds exposing (Bounds, Zoom)
import LatLng exposing (LatLng, getLat, getLng, latLng)
import Map
import Tile exposing (Offset)
import Utils exposing (flip)


initialResolution tileSize =
    2 * pi * 6378137 / tileSize


originShift =
    pi * 6378137


latLngMeters : LatLng -> Offset
latLngMeters latlng =
    let
        mx =
            getLng latlng * originShift / 180.0

        my_ =
            tan ((90 + getLat latlng) * pi / 360.0) |> logBase e |> flip (/) (pi / 180.0)

        my =
            my_ * originShift / 180.0
    in
    { x = mx, y = my }


unproject : Int -> Zoom -> Offset -> LatLng
unproject tileSize zoom { x, y } =
    let
        -- https://observablehq.com/@kjerandp/unprojecting-map-tile-coordinates
        r =
            6378137

        d =
            180 / pi

        s =
            0.5 / (pi * r)

        scale =
            toFloat tileSize * (2 ^ zoom)

        px =
            (x / scale - 0.5) / s

        py =
            (y / scale - 0.5) / -s
    in
    latLng
        { lat = (2 * atan (e ^ (py / r)) - (pi / 2)) * d
        , lng = px * d / r
        }


pixelToWorld : Zoom -> Offset -> Offset
pixelToWorld zoom { x, y } =
    let
        pToW p =
            p / (2 ^ zoom)
    in
    { x = pToW x, y = pToW y }


tileToLatLng : Offset -> Zoom -> LatLng
tileToLatLng { x, y } zoom =
    let
        sinh f =
            (e ^ f - e ^ -f) / 2

        n =
            2.0 ^ zoom

        lon_deg =
            x / n * 360.0 - 180.0

        lat_rad =
            atan (sinh (pi * (1 - 2 * y / n)))

        lat_deg =
            180 * (lat_rad / pi)
    in
    latLng { lat = lat_deg, lng = lon_deg }

-- https://developers.google.com/maps/documentation/javascript/coordinates#world-coordinates
pixelToLatLng : Int -> Float -> Offset -> LatLng
pixelToLatLng tileSize zoom offset =
    unproject tileSize 0 <| pixelToWorld zoom offset


metersPixels : Float -> Zoom -> Offset -> Offset
metersPixels tileSize zoom { x, y } =
    let
        res =
            resolution tileSize zoom

        px =
            (x + originShift) / res |> round

        py =
            (y + originShift) / res |> round
    in
    { x = toFloat px, y = toFloat py }


resolution : Float -> Zoom -> Float
resolution tileSize zoom =
    initialResolution tileSize / (2 ^ zoom)
