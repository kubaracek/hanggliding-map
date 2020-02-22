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
    = TextureLoaded MapType Tile.Offset (Maybe Texture)


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

        TextureLoaded maptype offset (Just texture) ->
            ( updateTexture (Success { offset = offset, texture = texture }) maptype
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
        ]
    }


viewCanvas : String -> MapType -> TexturedMap -> Html Msg
viewCanvas id maptype model =
    let
        clear =
            shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat model.map.width) (toFloat model.map.height) ]

        load ( url, offset ) =
            Texture.loadFromImageUrl url (TextureLoaded maptype offset)

        renderTile tex =
            case tex of
                Loading ->
                    Canvas.text [] ( 0, 0 ) "Loading"

                Success tt ->
                    Canvas.texture [] ( tt.offset.x, tt.offset.y ) tt.texture

                Failure ->
                    Canvas.text [] ( 0, 0 ) "Error"

        options =
            { width = model.map.width
            , height = model.map.height
            , textures = List.map load <| Map.tiles model.map
            }
    in
    Canvas.toHtmlWith options [Attr.id id] (clear :: List.map renderTile model.textures)
