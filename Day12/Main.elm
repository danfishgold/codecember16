module UrlParallax exposing (..)

import Html exposing (program)
import Helper
import Html exposing (Html, div, text)
import Window
import Mouse
import Navigation exposing (Location)
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


widthFromWindow : Window.Size -> Msg
widthFromWindow { width } =
    SetWidth (toFloat width)


fractionFromMouse : Model -> Mouse.Position -> Msg
fractionFromMouse { width } { x } =
    SetFraction (toFloat x / width)


init : Location -> ( Model, Cmd Msg )
init _ =
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
    , Task.perform widthFromWindow Window.size
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves (fractionFromMouse model)
        , Window.resizes widthFromWindow
        ]



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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWidth width ->
            ( { model | width = width }, Cmd.none )

        SetFraction f ->
            let
                oldUrl =
                    url 70 model.objects model.fraction

                newUrl =
                    url 70 model.objects f
            in
                ( { model | fraction = f }
                , -- if newUrl /= oldUrl then
                  --     Navigation.modifyUrl (baseUrl ++ newUrl)
                  --   else
                  Cmd.none
                )

        None ->
            ( model, Cmd.none )



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


main : Program Never Model Msg
main =
    Navigation.program (always None)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view |> Helper.project 12 description
        }
