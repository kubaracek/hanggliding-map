module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Canvas.Settings.Text exposing (..)
import Canvas.Texture as Texture exposing (Texture)
import Color
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (onClick)
import Map
import Tile
import Projection exposing (pixelToLatLng)
import LatLng exposing (getLat, getLng)
import Bounds

type Load a
    = Loading
    | Success a
    | Failure

type alias Model =
    {
        heightMap : TexturedMap
      , imageMap : TexturedMap
    }

type alias TexturedMap =
    { map : Map.Map
    , textures : List (Load TileData)
    }


type alias TileData =
    { tile : Tile.Tile
    , texture : Texture
    }


initialModel : Model
initialModel =
    {
        heightMap = { map = Map.exampleHeightMap
                   , textures = []
                    }
       , imageMap = { map = Map.exampleTexturedMap
                    , textures = []
                    }
    }

type MapType =
    HeightMap
  | ImageMap

type Msg
    = TextureLoaded MapType Tile.Tile (Maybe Texture)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        asHeightMapIn a b = { a | heightMap = b }
        asImageMapIn a b = { a | imageMap = b }
        asTexturesIn a b = { a | textures = b }
        updateTexture texture maptype =
            case maptype of
                HeightMap ->
                    texture :: model.heightMap.textures
                        |> asTexturesIn model.heightMap
                        |> asHeightMapIn model
                ImageMap ->
                    texture :: model.imageMap.textures
                        |> asTexturesIn model.imageMap
                        |> asImageMapIn model
    in
    case msg of
        TextureLoaded maptype _ Nothing ->
            ( updateTexture Failure maptype
            , Cmd.none
            )

        TextureLoaded maptype tile (Just texture) ->
            ( updateTexture (Success { tile = tile, texture = texture }) maptype
            , Cmd.none
            )


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Browser.Document Msg
view model =
    { title = "Hanggliding Map"
    , body =
        [ viewCanvas "imageMap" ImageMap model.imageMap
        , viewCanvas "heightMap" HeightMap model.heightMap
        , text <| Maybe.withDefault "Unknown" (Maybe.map String.fromFloat <| List.minimum (List.map (\ t -> t.offset.x) <| Map.tiles model.imageMap.map))
        ]
    }


viewCanvas : String -> MapType -> TexturedMap -> Html Msg
viewCanvas id maptype model =
    let
        clear =
            shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat model.map.width) (toFloat model.map.height) ]

        load : Tile.Tile -> Texture.Source Msg
        load tile =
            Texture.loadFromImageUrl tile.url (TextureLoaded maptype tile)

        renderTile tex =
            case tex of
                Loading ->
                    [Canvas.text [] ( 0, 0 ) "Loading"]

                Success tt ->
                    let
                        latlng t = pixelToLatLng model.map.tileSize (Map.zoom model.map) <|
                                 { x = ((toFloat t.coordinates.x) * toFloat model.map.tileSize)
                                 , y = ((toFloat t.coordinates.y) * toFloat model.map.tileSize)
                                 }
                        calibrationText t = "Lat: " ++ (String.fromFloat <| getLat <| latlng t) ++  ", Lng: " ++ (String.fromFloat <| getLng <| latlng t)
                        offset = tt.tile.offset
                        size = toFloat tt.tile.size
                        _ = Debug.log "tile" tt.tile
                    in
                    [ Canvas.texture [] ( offset.x, offset.y ) tt.texture
                    --
                    -- Calibration
                    --
                    , Canvas.text [] (0,0) ""
                    , Canvas.text [font { size = 10, family = "sans-serif" }] ( offset.x, offset.y + 10 ) <| "lat: " ++ (String.fromFloat <| getLat <| latlng tt.tile)
                    , Canvas.text [font { size = 10, family = "sans-serif" }] ( offset.x, offset.y + 20 ) <| "lng: " ++ (String.fromFloat <| getLng <| latlng tt.tile)
                    , shapes [ fill Color.green ] [ rect ( offset.x + 128 - 2, offset.y + 128 - 2 ) 4 4 ]
                    , shapes [ fill Color.red ] [ rect ( offset.x - 2, offset.y - 2 ) 4 4 ]
                    , shapes [ fill Color.red ] [ rect ( offset.x - 2 + size, offset.y - 2 ) 4 4 ]
                    , shapes [ fill Color.red ] [ rect ( offset.x - 2, offset.y - 2 + size ) 4 4 ]
                    , shapes [ fill Color.red ] [ rect ( offset.x - 2 + size, offset.y - 2 + size ) 4 4 ]
                    ]

                Failure ->
                    [Canvas.text [] ( 0, 0 ) "Error"]

        options =
            { width = model.map.width
            , height = model.map.height
            , textures = List.map load <| Map.tiles model.map
            }
    in
    Canvas.toHtmlWith options
        [ Attr.id id
        , Attr.hidden False
        -- , Attr.style "position" "absolute"
        , Attr.style "opacity" "1"
        , Attr.style "background-color" "red"
        ] (clear :: List.concatMap renderTile model.textures)
