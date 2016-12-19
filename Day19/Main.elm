module Matrix exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, g, text, text_)
import Svg.Attributes exposing (width, height)
import Time exposing (Time, second, every)
import Day19.Trail as Trail exposing (Trail)
import Random exposing (generate)
import Random.Extra exposing (combine)


type alias Model =
    { width : Float
    , height : Float
    , trails : List Trail
    , trailLength : Int
    , count : Int
    , fontSize : Float
    , fontFamily : String
    , time : Int
    }


type Msg
    = Tick Time
    | SetTrails (List Trail)
    | AddTrail Trail


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , trails = []
      , trailLength = 10
      , count = 10
      , fontSize = 12
      , fontFamily = "monospace"
      , time = 0
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    every (0.1 * second) Tick



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                time =
                    model.time + 1

                ( wd, ht ) =
                    ( model.width / model.fontSize |> floor, model.height / model.fontSize |> floor )

                trails =
                    model.trails |> List.filter (\{ letters, y } -> y - List.length letters <= ht)
            in
                ( { model | time = time }
                , Cmd.batch
                    [ if List.length model.trails < model.count then
                        generate AddTrail (Trail.random wd model.trailLength)
                      else
                        Cmd.none
                    , model.trails |> List.map (Trail.update model.time) |> combine |> generate SetTrails
                    ]
                )

        SetTrails trails ->
            ( { model | trails = trails }, Cmd.none )

        AddTrail trail ->
            ( { model | trails = trail :: model.trails }, Cmd.none )



--


view : Model -> Svg Msg
view model =
    model.trails
        |> List.map (Trail.view model.fontFamily model.fontSize)
        |> svg [ width <| toString model.width, height <| toString model.height ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
