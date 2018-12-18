port module Day07.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Color exposing (Color)
import Day02.Ryb exposing (ryba)
import Helper exposing (projectSvg)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Json
import Json.Encode exposing (Value)
import String
import Svg exposing (Svg, path, svg)
import Svg.Attributes exposing (d, fill, height, width)


type alias Model =
    { loops : List Loop
    , minLength : Int
    , maxLength : Int
    , width : Int
    , height : Int
    }


type Msg
    = SetLoops (List Loop)
    | RequestLoops


type alias Point =
    ( Int, Int )


type alias Loop =
    { deltas : List Point
    , center : Point
    , color : Color
    }


init : Int -> Int -> Int -> Int -> Int -> ( Model, Cmd Msg )
init width height minLength maxLength count =
    ( { loops = []
      , width = width
      , height = height
      , minLength = minLength
      , maxLength = maxLength
      }
    , requestLoops
        { width = width
        , height = height
        , minLength = minLength
        , maxLength = maxLength
        , count = count
        }
    )



--


description : String
description =
    """
**Originally from October 2014**

In 2014 someone showed me [snarXiv](http://snarxiv.org), which is a random scientific paper generator.
It uses [Backus-Naur Form](https://en.wikipedia.org/wiki/Backus–Naur_form)
to define the syntax and a perl script translates it to OCaml code.

I made a BNF parser in Python, which generated beautiful, disgusting, lazy JS code.
For some reason I decided to make a [random walk example](http://fishgold.co/BNF/paths).
The result was very pretty, so I wanted to reimplement it in a way that wasn't super weird.
"""


page =
    { init = always <| init 100 100 400 1400 20
    , subscriptions = subscriptions
    , update = update
    , title = "Loops"
    , body = view 5
    , description = description
    }



--


type alias JSLoop =
    ( List Point, Point, Float )


{-| width, height, minLength, maxLength, count
-}
port requestLoops :
    { width : Int
    , height : Int
    , minLength : Int
    , maxLength : Int
    , count : Int
    }
    -> Cmd msg


port getLoops : (List JSLoop -> msg) -> Sub msg


parseLoop : JSLoop -> Loop
parseLoop ( deltas, center, hue ) =
    { deltas = deltas
    , center = center
    , color = ryba hue 1 0.5 0.5
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    getLoops (List.map parseLoop >> SetLoops)



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetLoops loops ->
            ( { model | loops = loops }, Cmd.none )

        RequestLoops ->
            ( model
            , requestLoops
                { width = model.width
                , height = model.height
                , minLength = model.minLength
                , maxLength = model.maxLength
                , count = List.length model.loops
                }
            )



--


view : Float -> Model -> Svg Msg
view res model =
    let
        wd =
            toFloat model.width * res

        ht =
            toFloat model.height * res

        command cmd ( x, y ) =
            cmd
                ++ " "
                ++ String.fromFloat (toFloat x * res)
                ++ " "
                ++ String.fromFloat (toFloat y * res)

        pathD { deltas, center } =
            command "M" center
                :: (deltas |> List.map (command "l"))
                |> String.join " "

        loopPath loop =
            path
                [ fill <| Color.toCssString loop.color
                , d <| pathD loop
                ]
                []
    in
    div []
        [ model.loops
            |> List.map loopPath
            |> projectSvg ( wd, ht ) []
        , div [] [ button [ onClick RequestLoops ] [ text "Randomize" ] ]
        ]
