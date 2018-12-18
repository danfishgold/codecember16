module Helper exposing (filled, outlined, projectCollage, projectSvg, projectSvgAttrs)

import Array exposing (Array)
import Browser
import Browser.Events
import Collage exposing (Collage, LineJoin, Shape, defaultLineStyle)
import Collage.Render
import Color exposing (Color)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (href, style)
import Json.Decode as Json
import Markdown
import Svg exposing (Svg, svg)
import Svg.Attributes



-- SVG COLORS


filled color =
    Collage.filled <| Collage.uniform <| avh4ToTheSetColor color


outlined : Float -> Color -> LineJoin -> Shape -> Collage msg
outlined thickness color join =
    Collage.outlined
        { defaultLineStyle
            | thickness = thickness
            , join = join
            , fill = Collage.uniform <| avh4ToTheSetColor color
        }


avh4ToTheSetColor : Color -> { red : Int, green : Int, blue : Int, alpha : Float }
avh4ToTheSetColor c =
    let
        { red, green, blue, alpha } =
            Color.toRgba c
    in
    { red = floor <| red * 255
    , green = floor <| green * 255
    , blue = floor <| blue * 255
    , alpha = alpha
    }



-- SVG RENDER


viewBox x0 y0 wd ht =
    Svg.Attributes.viewBox <|
        String.fromFloat x0
            ++ " "
            ++ String.fromFloat y0
            ++ " "
            ++ String.fromFloat wd
            ++ " "
            ++ String.fromFloat ht


projectSvgAttrs : Bool -> ( Float, Float ) -> List (Html.Attribute msg)
projectSvgAttrs isCentered ( width, height ) =
    [ if isCentered then
        viewBox (-width / 2) (-height / 2) width height

      else
        viewBox 0 0 width height
    , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
    , style "width" "90vw"
    , style "max-width" "500px"
    , style "height" "auto"
    ]


projectCollage : ( Float, Float ) -> Collage msg -> Html msg
projectCollage size collage =
    Collage.Render.svgExplicit (projectSvgAttrs True size) collage


projectSvg : ( Float, Float ) -> List (Html.Attribute msg) -> List (Svg msg) -> Html msg
projectSvg size attrs children =
    svg (projectSvgAttrs False size ++ attrs) children
