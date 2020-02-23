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

unproject : Int -> Zoom -> Offset -> LatLng
unproject tileSize zoom {x, y} =
    let
        -- https://observablehq.com/@kjerandp/unprojecting-map-tile-coordinates
        r = 6378137
        d = 180 / pi
        s = 0.5 / (pi * r)
        scale = toFloat tileSize * (2 ^ zoom)
        py = (x / scale - 0.5) / s
        px = (y / scale - 0.5) / -s
    in
        latLng
          { lat = (2 * atan(e ^ (py / r)) - (pi / 2)) * d
          , lng = px * d / r
          }

pixelToLatLng : Int -> Float -> Offset -> LatLng
pixelToLatLng tileSize zoom {x, y} =
  unproject tileSize zoom {x = (x * toFloat tileSize), y = (y * toFloat tileSize)}

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
