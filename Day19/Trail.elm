module Day19.Trail exposing (..)

import Random
import Random.Extra
import Svg exposing (Svg, g, text, text_)
import Svg.Attributes exposing (x, y)


type alias Trail =
    {}


random : Random.Generator Trail
random =
    Random.Extra.constant {}


view : Trail -> Svg msg
view trail =
    g [] [ text_ [ x "0", y "0" ] [ text "yo" ] ]
