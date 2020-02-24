module Map exposing (..)

import Bounds exposing (Bounds(..), Zoom)
import LatLng exposing (LatLng, latLng)
import Maybe
import Projection exposing (pixelToLatLng)
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
        z =
            zoom map
    in
    1 + z - (toFloat <| floor z)


zoom : Map -> Zoom
zoom map =
    Bounds.zoom (toFloat map.tileSize) (toFloat map.width) (toFloat map.height) map.bounds


tiles : Map -> List Tile
tiles map =
    let
        z =
            zoom map

        center =
            Bounds.findCenter map.bounds

        xCount =
            toFloat map.width / toFloat map.tileSize

        yCount =
            toFloat map.height / toFloat map.tileSize

        tile =
            Tile.fromLatLng (toFloat <| ceiling z) center

        xTiles =
            List.range (floor <| -xCount / 2) (ceiling <| xCount / 2)

        yTiles =
            List.range (floor <| -yCount / 2) (ceiling <| yCount / 2)

        wrapTile =
            wrap 0 (2 ^ ceiling z)

        tileCoords t x y =
            { x = floor t.x + x |> wrapTile
            , y = floor t.y + y |> wrapTile
            }

        tileXY x y =
            { url =
                Tile.url
                    map.server
                    (ceiling z)
                    (tileCoords tile x y).x
                    (tileCoords tile x y).y
            , offset =
                Tile.Offset
                    (toFloat map.width / 2 + (toFloat (floor tile.x) - tile.x + toFloat x) * toFloat map.tileSize)
                    (toFloat map.height / 2 + (toFloat (floor tile.y) - tile.y + toFloat y) * toFloat map.tileSize)
            , coordinates = tileCoords tile x y
            , size = map.tileSize
            }
    in
    List.concat <| cartesianMap tileXY xTiles yTiles


setHeight : Int -> Map -> Map
setHeight res map =
    { map | height = res }


setWidth : Int -> Map -> Map
setWidth res map =
    { map | width = res }



---- GEO STUFF
----
-- Get visible's map bounds lat/lng
-- (what user sees)
getVisibleBounds : Map -> Bounds LatLng
getVisibleBounds map =
    let
        firstTile =
            List.head <| tiles map

        -- Get's coordinates of the first pixel that user sees (top-left)
        firstTileScreenPixelCord =
            case firstTile of
                Just tile ->
                    { x = toFloat (tile.coordinates.x * map.tileSize) - tile.offset.x
                    , y = toFloat (tile.coordinates.y * map.tileSize) - tile.offset.y
                    }

                Nothing ->
                    { x = 0, y = 0 }

        screenSwPixel =
            { x = firstTileScreenPixelCord.x
            , y = firstTileScreenPixelCord.y + toFloat map.height
            }

        screenNePixel =
            { x = firstTileScreenPixelCord.x + toFloat map.width
            , y = firstTileScreenPixelCord.y
            }
    in
    Bounds
        { northEast = pixelToLatLng map.tileSize (zoom map) screenNePixel
        , southWest = pixelToLatLng map.tileSize (zoom map) screenSwPixel
        }
