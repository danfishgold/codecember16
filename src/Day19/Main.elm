module Day19.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Day19.Trail as Trail exposing (Trail)
import Random exposing (generate)
import Random.Extra exposing (combine)
import Svg exposing (Svg, g, rect, svg, text, text_)
import Svg.Attributes exposing (fill, height, width, x, y)
import Time exposing (Posix, every)


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
    = Tick Posix
    | SetTrails (List Trail)
    | AddTrail Trail


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , trails = []
      , trailLength = 15
      , count = 15
      , fontSize = 12
      , fontFamily = "monospace"
      , time = 0
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    every 10 Tick



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
                [ if List.length trails < model.count then
                    generate AddTrail (Trail.random wd model.trailLength)

                  else
                    Cmd.none
                , trails |> List.map (Trail.update model.time) |> combine |> generate SetTrails
                ]
            )

        SetTrails trails ->
            ( { model | trails = trails }, Cmd.none )

        AddTrail trail ->
            ( { model | trails = trail :: model.trails }, Cmd.none )



--


view : Model -> Svg Msg
view model =
    [ rect
        [ x <| "0"
        , y <| "0"
        , width <| String.fromFloat model.width
        , height <| String.fromFloat model.height
        , fill "black"
        ]
        []
    , model.trails
        |> List.map (Trail.view model.fontFamily model.fontSize)
        |> g []
    ]
        |> svg [ width <| String.fromFloat model.width, height <| String.fromFloat model.height ]



--


description : String
description =
    """
Around 2008 I found an series of articles about development for beginners.
It was written by [Ido Gendel](http://www.idogendel.com/),
its name was a pun, and it discussed all sorts of topics. It was great.
Almost 10 years later I still remember it and I still think it's great.

There are two articles I especially liked.
[This](http://net.nana10.co.il/Article/?ArticleID=613745) is one of them,
and this project is a reimplementation of it. The second one is yet to come.
"""


page =
    { init = always <| init 500 500
    , subscriptions = subscriptions
    , update = update
    , title = "Matrix"
    , body = view
    , description = description
    }
