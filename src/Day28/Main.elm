module Day28.Main exposing (Model, Msg, page)

import Day27.Main278 as M
import Day28.Border as Border


type alias Model =
    M.Model


type alias Msg =
    M.Msg


init =
    M.init "day28"


update =
    M.update "day28"


view =
    M.view "day28" (\model fill -> Border.view model.gridSize.scale fill)


subscriptions =
    M.subscriptions



--


description : String
description =
    """
In my opinion this is cooler than yesterday's project.

## Instructions

Click and drag to make shape outlines.
You can use the "Cross" option to make this faster.
Disconnected areas automatically get a different color. When two outlines meet
they merge.
"""


page =
    { init = always <| init 50 50 10
    , subscriptions = subscriptions
    , update = update
    , title = "Borders"
    , body = view
    , description = description
    }
