module Main exposing (main)

import Array
import Browser
import Collage exposing (Collage, filled, rectangle, uniform)
import Collage.Layout exposing (horizontal, vertical)
import Collage.Render exposing (svg)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List.Extra exposing (groupsOf)
import MD5


type alias Model =
    { name : String
    , identicon : Collage Msg
    }


initialModel : Model
initialModel =
    { name = ""
    , identicon =
        initialIdenticon
    }


initialIdenticon : Collage Msg
initialIdenticon =
    rectangle 250 250
        |> filled (uniform Color.white)


type Msg
    = SetName String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetName name ->
            let
                newIdenticon =
                    case name of
                        "" ->
                            initialIdenticon

                        _ ->
                            imageFromName name
            in
            { model | name = name, identicon = newIdenticon }


imageFromName : String -> Collage msg
imageFromName name =
    let
        bytes =
            MD5.bytes name

        color =
            bytes
                |> Array.fromList
                |> Array.slice 0 3
                |> Array.toList

        grid =
            bytes
                |> groupsOf 3
                |> List.map mirrorRow
    in
    grid
        |> List.map (toCollage color)
        |> vertical


mirrorRow : List a -> List a
mirrorRow row =
    case row of
        first :: second :: _ ->
            row ++ [ second, first ]

        _ ->
            row


toCollage : List Int -> List Int -> Collage msg
toCollage color row =
    row
        |> List.map (toSquare color)
        |> horizontal


toSquare : List Int -> Int -> Collage msg
toSquare color value =
    let
        fill =
            case color of
                [ r, g, b ] ->
                    if isEven value then
                        Color.rgb255 r g b

                    else
                        Color.white

                _ ->
                    Color.white
    in
    rectangle 50 50
        |> filled (uniform fill)


isEven : Int -> Bool
isEven number =
    modBy 2 number == 0


view : Model -> Html Msg
view model =
    div []
        [ viewIdenticon model.identicon
        , input [ onInput SetName ] []
        ]


viewIdenticon : Collage Msg -> Html Msg
viewIdenticon identicon =
    identicon |> svg


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }
