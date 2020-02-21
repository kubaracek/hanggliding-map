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
    { map : Map.Map
    , textures : List (Load TileData)
    }


type alias TileData =
    { offset : Tile.Offset
    , texture : Texture
    }


initialModel : Model
initialModel =
    { map = Map.initialModel, textures = [] }


type Msg
    = TextureLoaded Tile.Offset (Maybe Texture)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextureLoaded _ Nothing ->
            ( { model | textures = Failure :: model.textures }
            , Cmd.none
            )

        TextureLoaded offset (Just texture) ->
            ( { model
                | textures =
                    Success
                        { offset = offset, texture = texture }
                        :: model.textures
              }
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
        [ viewCanvas model
        ]
    }


viewCanvas : Model -> Html Msg
viewCanvas model =
    let
        clear =
            shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat model.map.width) (toFloat model.map.height) ]

        load ( url, offset ) =
            Texture.loadFromImageUrl url (TextureLoaded offset)

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
    Canvas.toHtmlWith options [] (clear :: List.map renderTile model.textures)
