module Map exposing (..)

import Bounds exposing (Zoom)
import LatLng exposing (LatLng)

type alias Map =
    { server : String
    , center : LatLng
    , zoom : Zoom
    , width : Int
    , height : Int
    , tileSize : Int
    }
