module LatLng exposing (GPS, LatLng, fromGps, latLng)


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
fromGps { lat, lng } =
    LatLng
        { lat = decGps lat
        , lng = decGps lng
        }


getLat : LatLng -> Float
getLat latlng =
    case latlng of
        LatLng { lat, lng } ->
            lat

        Gps { lat, lng } ->
            decGps lat


getLng : LatLng -> Float
getLng latlng =
    case latlng of
        LatLng { lat, lng } ->
            lng

        Gps { lat, lng } ->
            decGps lng
