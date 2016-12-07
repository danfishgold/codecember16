port module Loops exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, path)
import Svg.Attributes exposing (width, height, fill, d)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgba)
import Day2.Ryb exposing (ryba)
import String


type alias Model =
    { loops : List Loop
    , minLength : Int
    , maxLength : Int
    , width : Int
    , height : Int
    }


type Msg
    = SetLoops (List Loop)


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
    , requestLoops ( width, height, minLength, maxLength, count )
    )



--


main : Program Never Model Msg
main =
    program
        { init = init 50 50 100 400 20
        , subscriptions = subscriptions
        , update = update
        , view = view 10
        }



--


type alias JSLoop =
    ( List Point, Point, Float )


{-| width, height, minLength, maxLength, count
-}
port requestLoops : ( Int, Int, Int, Int, Int ) -> Cmd msg


port getLoops : (List JSLoop -> msg) -> Sub msg


parseLoop : JSLoop -> Loop
parseLoop ( deltas, center, hue ) =
    { deltas = deltas
    , center = center
    , color = ryba (hue) 1 0.5 0.5
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
                ++ toString (toFloat x * res)
                ++ " "
                ++ toString (toFloat y * res)

        pathD { deltas, center } =
            (command "M" center)
                :: (deltas |> List.map (command "l"))
                |> String.join " "

        loopPath loop =
            path
                [ fill <| colorToCssRgba loop.color
                , d <| pathD loop
                ]
                []
    in
        model.loops
            |> List.map loopPath
            |> svg [ width <| toString wd, height <| toString ht ]
