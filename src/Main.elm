module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Map
import Tile


type alias Model =
    Map.Map


initialModel : Model
initialModel =
    Map.initialModel


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model, Cmd.none )

        Decrement ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Hanglider Map"
    , body =
        [ div
            [ Attr.style "width" <| String.fromInt model.width ++ "px"
            , Attr.style "height" <| String.fromInt model.height ++ "px"
            , Attr.style "background-color" "red"
            ]
          <|
            List.map (\(( url, offset ) as tile) -> Tile.view (toFloat model.tileSize) tile) <|
                Map.tiles model
        ]
    }


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
