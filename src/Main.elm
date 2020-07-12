module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Image exposing (Image)
import List.Extra exposing (..)
import MD5 exposing (..)


type alias Model =
    { name : String
    }


initialModel : Model
initialModel =
    { name = ""
    }


type Msg
    = SetName String
    | CreateIdenticon


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetName name ->
            { model
                | name = name
            }

        CreateIdenticon ->
            model


imageFromName : String -> Image
imageFromName name =
    let
        hash =
            MD5.hex name

        byteGroups =
            chunk 2 hash

        colorHex =
            hash
                |> String.left 6
    in



chunk : Int -> String -> List String
chunk size string =
    string
        |> String.toList
        |> List.map (\character -> String.cons character "")
        |> List.Extra.groupsOf size
        |> List.map String.join ""


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput SetName ] []
        ]


viewIdenticon : Image -> Html Msg
viewIdenticon imageUrl =
    img [ src imageUrl ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }
