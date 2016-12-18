module Day18.Gradient exposing (gradient, gradientStroke)

import Svg exposing (Svg, defs, stop)
import Svg.Attributes exposing (id, x1, y1, x2, y2, stopColor, offset)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
