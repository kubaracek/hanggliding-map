module Projection exposing (..)

import Bounds exposing (Bounds, Zoom)
import LatLng exposing (LatLng, getLat, getLng, latLng)
import Tile exposing (Offset)
import Utils exposing (flip)
import Map


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
