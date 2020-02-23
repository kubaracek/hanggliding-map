module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Canvas.Texture as Texture exposing (Texture)
import Color
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (onClick)
import Map
import Tile


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
    { offset : Tile.Offset
    , size : Float
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
    = TextureLoaded Float MapType Tile.Offset (Maybe Texture)


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
        TextureLoaded _ maptype _ Nothing ->
            ( updateTexture Failure maptype
            , Cmd.none
            )

        TextureLoaded size maptype offset (Just texture) ->
            ( updateTexture (Success { offset = offset, texture = texture, size = size }) maptype
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
        , text <| Maybe.withDefault "Unknown" (Maybe.map String.fromFloat <| List.minimum (List.map (\ (_, t) -> t.x) <| Map.tiles model.imageMap.map))
        ]
    }


viewCanvas : String -> MapType -> TexturedMap -> Html Msg
viewCanvas id maptype model =
    let
        clear =
            shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat model.map.width) (toFloat model.map.height) ]

        load ( url, offset ) =
            Texture.loadFromImageUrl url (TextureLoaded (toFloat model.map.tileSize)  maptype offset)

        renderTile tex =
            case tex of
                Loading ->
                    [Canvas.text [] ( 0, 0 ) "Loading"]

                Success tt ->
                    [ Canvas.texture [] ( tt.offset.x, tt.offset.y ) tt.texture
                    --
                    -- Calibration
                    --
                    , Canvas.text [] ( tt.offset.x - 2, tt.offset.y - 2 ) "hello"
                    , shapes [ fill Color.red ] [ rect ( tt.offset.x - 2, tt.offset.y - 2 ) 4 4 ]
                    , shapes [ fill Color.red ] [ rect ( tt.offset.x - 2 + tt.size, tt.offset.y - 2 ) 4 4 ]
                    , shapes [ fill Color.red ] [ rect ( tt.offset.x - 2, tt.offset.y - 2 + tt.size ) 4 4 ]
                    , shapes [ fill Color.red ] [ rect ( tt.offset.x - 2 + tt.size, tt.offset.y - 2 + tt.size ) 4 4 ]
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
        , Attr.style "position" "absolute"
        , Attr.style "opacity" "0.5"
        , Attr.style "background-color" "red"
        ] (clear :: List.concatMap renderTile model.textures)
