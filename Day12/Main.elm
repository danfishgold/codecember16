module Day12.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Dom exposing (getViewport)
import Browser.Events as Events
import Helper
import Html exposing (Html, div, text)
import Json.Decode as Json
import Task


type alias Object =
    { str : String
    , f : Float
    , z : Float
    }


type alias Model =
    { width : Float
    , fraction : Float
    , objects : List Object
    }


type Msg
    = SetWidth Float
    | SetFraction Float
    | None


init : ( Model, Cmd Msg )
init =
    ( { width = 1
      , fraction = 0.5
      , objects =
            [ Object "|‾‾‾‾‾‾|" 0.5 1
            , Object "|‾‾‾|" 0.1 1
            , Object "┌──┐" 0.1 4
            , Object "┌┐" -2 5
            , Object "┌┐" 1.1 6
            ]
      }
    , getViewport
        |> Task.map (\{ viewport } -> viewport.width)
        |> Task.perform SetWidth
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onMouseMoves model SetFraction
        , onWindowResize SetWidth
        ]


onMouseMoves model toMsg =
    Events.onMouseMove
        (Json.field "x" Json.float
            |> Json.map (\x -> x / model.width)
            |> Json.map toMsg
        )


onWindowResize toMsg =
    Events.onResize (\wd ht -> toMsg (toFloat wd))



--


project : Float -> Object -> ( Object, Float )
project f0 ({ f, z } as object) =
    ( object, 0.5 + (f0 - f) / z )


url : Int -> List Object -> Float -> String
url n objects f0 =
    let
        base =
            String.repeat n "_"

        addToString ( obj, f ) str =
            let
                k =
                    floor <| f * toFloat n

                before =
                    str |> String.left k

                after =
                    str |> String.dropLeft (k + String.length obj.str)
            in
            before ++ obj.str ++ after
    in
    objects
        |> List.sortBy .z
        |> List.map (project f0)
        |> List.foldr addToString base


baseUrl : String
baseUrl =
    "http://fishgold.co/codecember16/Day12/"


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetWidth width ->
            { model | width = width }

        SetFraction f ->
            { model | fraction = f }

        None ->
            model



--


view : Model -> Html Msg
view model =
    div [] [ text <| url 70 model.objects model.fraction ]



--


description : String
description =
    """
After I saw the parallax in GitHub's 404 page (see previous day), I found
a jQuery library for parallax, and it even had parallax in the url.
I tried to do the same but I'm not sure I succeeded.

This was pretty much the most disappointing day.

## Instructions

Move the mouse ¯\\\\\\_(ツ)\\_/¯
"""


page =
    { init = always init
    , subscriptions = subscriptions
    , update = \msg model -> ( update msg model, Cmd.none )
    , title = "ASCII Parallax"
    , body = view
    , description = description
    }
