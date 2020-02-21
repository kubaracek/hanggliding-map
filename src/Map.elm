module Map exposing (..)

import Bounds exposing (Bounds, Zoom)
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

{-| Test:
map =
{ server = "<http://c.tile.stamen.com/watercolor/${z}/${x}/${y}.jpg">
, bounds =
{ northEast = latLng { lat=48.874538, lng=17.134638 }
, southWest = latLng { lat=48.839470, lng=17.113760 }
}
, width = 1024
, height = 1024
, tileSize = 256
}
-}
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
