module Day27.Main exposing (Model, Msg, page)

import Day27.Area as Area
import Day27.Main278 as M


type alias Model =
    M.Model


type alias Msg =
    M.Msg


init =
    M.init "day27"


update =
    M.update "day27"


view =
    M.view "day27" (\model _ -> Area.view model.gridSize.scale)


subscriptions =
    M.subscriptions



--


description : String
description =
    """
This seemed like a cool concept.
I was actually going for what I accomplished in the next day, but it took some time.

I tried several implementations but I think I like the result.

This isn't inspired by anything specific.
Nothing is as it seems and everything is random.

## Instructions

Click and drag to make shapes.
You can use the "Cross" option to make this faster.
Disconnected areas automatically get a different color. When two areas meet
they merge.
"""


page =
    { init = always <| init 50 50 10
    , subscriptions = always Sub.none
    , update = update
    , title = "Areas"
    , body = view
    , description = description
    }
