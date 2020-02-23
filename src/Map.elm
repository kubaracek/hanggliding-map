module Map exposing (..)

import Bounds exposing (Bounds(..), Zoom)
import LatLng exposing (LatLng, latLng)
import Tile exposing (Tile)
import Utils exposing (cartesianMap, wrap)


type alias Map =
    { server : String
    , bounds : Bounds LatLng
    , width : Int
    , height : Int
    , tileSize : Int
    }

exampleHeightMap =
    { server = "https://tile.nextzen.org/tilezen/terrain/v1/256/normal/{z}/{x}/{y}.png?api_key=XJJmLW0zTjKgdFUvWgLV7Q"
    , bounds =
        -- Bounds { northEast = latLng { lat = 47.46453992268503, lng = 11.716575622558594 }
        -- , southWest = latLng { lat = 47.253135632244216, lng = 11.376686096191406 }
        -- }
        Centered { center = latLng { lat = 47.347858, lng = 11.707069 }, zoom = 11 }
    , width = 1024
    , height = 1024
    , tileSize = 256
    }

exampleTexturedMap =
    { server = "https://api.maptiler.com/maps/topo/256/{z}/{x}/{y}.png?key=uOnJe75yrTX7TmDr3V5B"
    , bounds =
        -- Bounds { northEast = latLng { lat = 47.46453992268503, lng = 11.716575622558594 }
        -- , southWest = latLng { lat = 47.253135632244216, lng = 11.376686096191406 }
        -- }
        Centered { center = latLng { lat = 47.347858, lng = 11.707069 }, zoom = 11 }
    , width = 1024
    , height = 1024
    , tileSize = 256
    }

scaleFactor : Map -> Float
scaleFactor map =
    let
        zoom = Bounds.zoom (toFloat map.tileSize) (toFloat map.width) (toFloat map.height) map.bounds
    in
        1 + zoom - (toFloat <| floor zoom)

tiles : Map -> List Tile
tiles map =
    let
        zoom =
            Bounds.zoom (toFloat map.tileSize) (toFloat map.width) (toFloat map.height) map.bounds

        center =
            Bounds.findCenter map.bounds

        xCount =
            toFloat map.width / toFloat map.tileSize

        yCount =
            toFloat map.height / toFloat map.tileSize

        tile =
            Tile.fromLatLng (toFloat <| ceiling zoom) center

        xTiles =
            List.range (floor <| -xCount / 2) (ceiling <| xCount / 2)

        yTiles =
            List.range (floor <| -yCount / 2) (ceiling <| yCount / 2)

        wrapTile =
            wrap 0 (2 ^ ceiling zoom)

        tileXY x y =
            ( Tile.url
                map.server
                (ceiling zoom)
                (floor tile.x + x |> wrapTile)
                (floor tile.y + y |> wrapTile)
            , Tile.Offset
                (toFloat map.width / 2 + (toFloat (floor tile.x) - tile.x + toFloat x) * toFloat map.tileSize)
                (toFloat map.height / 2 + (toFloat (floor tile.y) - tile.y + toFloat y) * toFloat map.tileSize)
            )
    in
    List.concat <| cartesianMap tileXY xTiles yTiles


setHeight : Int -> Map -> Map
setHeight res map =
    { map | height = res }


setWidth : Int -> Map -> Map
setWidth res map =
    { map | width = res }
