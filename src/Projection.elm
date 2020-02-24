module Projection exposing (..)

import Bounds exposing (Zoom)
import LatLng exposing (LatLng, getLat, getLng, latLng)
import Tile exposing (Offset)
import Utils exposing (flip)


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

latLngToPixel : Float -> Zoom -> LatLng -> Offset
latLngToPixel tileSize zoom latlng =
    metersPixels tileSize zoom <| latLngMeters latlng

initialResolution : Float -> Float
initialResolution tileSize =
    2 * pi * 6378137 / tileSize

originShift : Float
originShift =
    pi * 6378137

resolution : Float -> Zoom -> Float
resolution tileSize zoom =
    initialResolution tileSize / (2 ^ zoom)


{-| Calculate the distance in kilometers between two points.
Note that this assumes the earth is spherical, which is not true, but may be true enough for your purposes.
-}
distanceBetween : LatLng  -> LatLng -> Float
distanceBetween a b =
    let
        earthRadius =
            6371

        dlat =
            degrees (getLat b - getLat a)

        dlng =
            degrees (getLng b - getLng a)

        v1 =
            sin (dlat / 2)
                * sin (dlat / 2)
                + cos (degrees <| getLat a)
                * cos (degrees <| getLat b)
                * sin (dlng / 2)
                * sin (dlng / 2)

        v2 =
            2 * atan2 (sqrt v1) (sqrt (1 - v1))
    in
    earthRadius * v2


{-| Calculate the heading you'd need to travel on to get from point a to point b.
-}
bearingTo : LatLng -> LatLng -> Float
bearingTo a b =
    let
        dlon =
            degrees (getLng b) - degrees (getLng a)

        y =
            sin dlon * cos (degrees <| getLat b)

        x =
            (cos (degrees <| getLat a) * sin (degrees <| getLat b)) - (sin (degrees <| getLat a) * cos (degrees <| getLat b) * cos dlon)

        bearing =
            atan2 y x * (180 / pi)
    in
    bearing
