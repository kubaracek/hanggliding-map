module LatLng exposing
    ( GPS(..)
    , LatLng
    , bearingTo
    , distanceBetween
    , fromGps
    , getLat
    , getLng
    , latLng
    )


type LatLng
    = LatLng
        { lat : Float
        , lng : Float
        }
    | Gps
        { lat : GPS
        , lng : GPS
        }


type GPS
    = GPS Float Float


latLng : { lat : Float, lng : Float } -> LatLng
latLng latlng =
    LatLng latlng


{-| test:
n = GPS 49 34.658
e = GPS 17 39.872
jerryTakeOff = {lat = n, lng = e}

    LatLng should be
        (49.577633, 17.664533)
    Full GPS data should be
        49° 34' 39.4788'' N
        17° 39' 52.3188'' E

    Doublecheck on https://www.latlong.net/Show-Latitude-Longitude.html

-}
decGps : GPS -> Float
decGps gps =
    case gps of
        GPS degree minute ->
            degree + (minute / 60)


fromGps : { lat : GPS, lng : GPS } -> LatLng
fromGps latlng =
    LatLng
        { lat = decGps latlng.lat
        , lng = decGps latlng.lng
        }


getLat : LatLng -> Float
getLat latlng =
    case latlng of
        LatLng l ->
            l.lat

        Gps l ->
            decGps l.lat


getLng : LatLng -> Float
getLng latlng =
    case latlng of
        LatLng l ->
            l.lng

        Gps l ->
            decGps l.lng


{-| Calculate the distance in kilometers between two points.
Note that this assumes the earth is spherical, which is not true, but may be true enough for your purposes.
-}
distanceBetween : LatLng -> LatLng -> Float
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
