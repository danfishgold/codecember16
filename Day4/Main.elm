module Frequency exposing (..)

import Html exposing (program)
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


view : String -> Model -> Html Msg
view side { events } =
    let
        eventDiv title opacity =
            div
                [ style
                    [ ( "width", side )
                    , ( "height", side )
                    , ( "flex", "0 1 auto" )
                    , ( "opacity", toString <| opacity )
                    , ( "display", "flex" )
                    , ( "text-align", "center" )
                    , ( "align-items", "center" )
                    , ( "justify-content", "center" )
                    ]
                ]
                [ text title ]

        event { title, duration, current, fired } =
            if not fired then
                eventDiv title 0
            else
                eventDiv title <| decay <| 5 * current / duration
    in
        events
            |> List.map event
            |> div
                [ style
                    [ ( "width", "500px" )
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
    [ ( "Heartbeat", 1 * second )
    , ( "One Birth", 1 * second / 2.5 )
    , ( "One Death", 1 * second / 2.3 )
    ]


main : Program Never Model Msg
main =
    program
        { init = init events
        , subscriptions = subscriptions
        , update = update
        , view = view "100px"
        }
