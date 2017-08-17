module Frequency exposing (..)

import Html exposing (program)
import Helper exposing (project)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import AnimationFrame exposing (diffs)
import Time exposing (Time, second)


type alias Model =
    { events : List { title : String, duration : Float, current : Time, fired : Bool } }


type Msg
    = Tick Time


init : List ( String, Float ) -> ( Model, Cmd Msg )
init events =
    let
        addInitials ( title, duration ) =
            { title = title, duration = duration, current = 0, fired = False }
    in
        ( { events = events |> List.map addInitials
          }
        , Cmd.none
        )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    diffs Tick



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                progress ({ current, duration } as event) =
                    if current + dt > duration then
                        { event | current = current + dt - duration, fired = True }
                    else
                        { event | current = current + dt }
            in
                ( { model | events = model.events |> List.map progress }, Cmd.none )



--


view : String -> String -> Model -> Html Msg
view width height { events } =
    let
        eventDiv title opacity =
            div
                [ style
                    [ ( "width", width )
                    , ( "height", height )
                    , ( "flex", "0 1 auto" )
                    , ( "opacity", toString <| opacity )
                    , ( "display", "flex" )
                    , ( "text-align", "center" )
                    , ( "align-items", "center" )
                    , ( "justify-content", "center" )
                    , ( "padding", "10px 10px" )
                    ]
                ]
                [ text title ]

        event { title, duration, current, fired } =
            if not fired then
                eventDiv title 0
            else
                eventDiv title <| decay <| current / (0.25 * second)
    in
        events
            |> List.map event
            |> div
                [ style
                    [ ( "width", "95%" )
                    , ( "max-width", "900px" )
                    , ( "margin", "0 auto" )
                    , ( "display", "flex" )
                    , ( "flex-wrap", "wrap" )
                    ]
                ]


decay : Time -> Float
decay f =
    0.15 + 0.85 * e ^ -f



--


events : List ( String, Float )
events =
    -- via https://www.explainxkcd.com/wiki/index.php/1331:_Frequency
    [ ( "Heartbeat", 0.86 * second )
    , ( "One Birth", 0.24 * second )
    , ( "One Death", 0.56 * second )
    , ( "Someone edits Wikipedia", 0.67 * second )
    , ( "Someone buys a vibrator", 2.99 * second )
    , ( "China builds a car", 1.89 * second )
    , ( "Japan builds a car", 4.01 * second )
    , ( "Germany builds a car", 5.8 * second )
    , ( "The US builds a car", 6.95 * second )
    , ( "Someone else builds a car", 1.03 * second )
    , ( "A European Union resident has their first kiss", 5.53 * second )
    , ( "A US fire department puts out a fire", 23 * second )
    , ( "Someone hits a hole in one", 180 * second )
    , ( "My turn signal blinks", 0.94 * second )
    , ( "The turn signal of the car in front of me blinks", 0.9 * second )
    , ( "Earthquake (magnitude 1)", 2.43 * second )
    , ( "Earthquake (magnitude 2)", 24.26 * second )
    , ( "Earthquake (magnitude 3)", 242.6 * second )
    , ( "Earthquake (magnitude 4)", 2426 * second )
    , ( "A member of the UK parliament flushes a toilet", 10.06 * second )
    , ( "An airline flight takes off", 0.93 * second )
    , ( "Someone buys To Kill a Mockingbird", 42.05 * second )
    , ( "Someone's pet cat kills a mockingbird", 1.82 * second )
    , ( "Someone in Phoenix buys new shoes", 1.08 * second )
    , ( "Someone in Phoenix puts on a condom", 2.05 * second )
    , ( "Someone locks their keys in their car", 2.43 * second )
    , ( "A Sagittarius named Amelia drinks a soda", 7.79 * second )
    , ( "A dog bites someone in the US", 7.01 * second )
    , ( "Someone steals a bicycle", 24.93 * second )
    , ( "A bald eagle catches a fish", 2.69 * second )
    , ( "50,000 plastic bottles are produced", 1.27 * second )
    , ( "50,000 plastic bottles are recycled", 4.64 * second )
    , ( "A bright meteor is visible somewhere", 1.15 * second )
    , ( "Old Faithful erupts", 5640 * second )
    , ( "A fishing boat catches a shark", 0.83 * second )
    , ( "Someone in the US is diagnosed with cancer", 18.99 * second )
    , ( "Someone in the US dies from cancer", 54.34 * second )
    , ( "Someone adopts a dog from a shelter", 15.6 * second )
    , ( "Someone adopts a cat from a shelter", 21.3 * second )
    , ( "Someone gets married", 0.75 * second )
    , ( "Someone registers a domain", 0.64 * second )
    , ( "Someone in the US buys a house", 6.22 * second )
    , ( "Someone in the US gets a tattoo", 2.06 * second )
    , ( "The star PSR J1748-2446AD rotates 1,000 times", 1.4 * second )
    , ( "Someone lies about their age to sign up for Facebook", 4.32 * second )
    , ( "Someone breaks an iPhone screen", 0.93 * second )
    , ( "A Little League player strikes out", 1.23 * second )
    , ( "Someone has sex in North Dakota", 1.38 * second )
    , ( "Justin Bieber gains a follower on Twitter", 4.73 * second )
    , ( "Someone in Denver orders pizza", 1.27 * second )
    ]



--


description : String
description =
    """
**Originally from 2013**

This is a reimplementation of [this](https://xkcd.com/1331/) by Randall Monroe.

In 2013 I implemented it in Mathematica, which was probably the weirdest choice anyone could make.
"""


main : Program Never Model Msg
main =
    program
        { init = init events
        , subscriptions = subscriptions
        , update = update
        , view = view "150px" "45px" |> project 4 description
        }
