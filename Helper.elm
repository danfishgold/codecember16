module Helper exposing (filled, onEnter, onEnterOrSpace, outlined, project)

import Array exposing (Array)
import Browser
import Browser.Events
import Collage exposing (Collage, LineJoin, Shape, defaultLineStyle)
import Color exposing (Color)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (href, style)
import Json.Decode as Json
import Markdown



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



-- KEYBOARD EVENTS


onEnter toMsg =
    Browser.Events.onKeyUp
        (Json.field "key" Json.string
            |> Json.andThen
                (\key ->
                    if key == "Enter" then
                        Json.succeed toMsg

                    else
                        Json.fail "Other key"
                )
        )


onEnterOrSpace enterMsg spaceMsg =
    Browser.Events.onKeyUp
        (Json.field "key" Json.string
            |> Json.andThen
                (\key ->
                    case key of
                        "Enter" ->
                            Json.succeed enterMsg

                        " " ->
                            Json.succeed spaceMsg

                        _ ->
                            Json.fail "Other key pressed"
                )
        )



-- VIEW


title : Int -> Html msg
title day =
    case Array.get (day - 1) titles of
        Nothing ->
            Debug.todo "No day like the present"

        Just str ->
            h1 [ style "padding" "5px 30px" ] [ text <| "Day " ++ String.fromInt day ++ ": " ++ str ]


link : Int -> Html msg
link day =
    case Array.get (day - 1) titles of
        Nothing ->
            a [] []

        Just str ->
            a [ href <| "http://fishgold.co/codecember16/Day" ++ String.fromInt day ]
                -- a [ href <| "http://localhost:8000/Day" ++ String.fromInt day ++ "/Main.elm" ]
                [ text <| "Day " ++ String.fromInt day ++ ": " ++ str ]


project : Int -> String -> (model -> Html msg) -> model -> Browser.Document msg
project day description proj model =
    let
        today =
            Array.get (day - 1) titles

        yesterday =
            Array.get (day - 2) titles

        tomorrow =
            Array.get day titles

        header =
            div
                [ style "display" "flex"
                , style "flex-direction" "row"
                , style "align-items" "baseline"
                , style "justify-content" "center"
                ]
                [ link (day - 1), title day, link (day + 1) ]
    in
    { body =
        [ div [ style "font-family" "sans-serif" ]
            [ div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "align-items" "center"
                ]
                [ div [ style "padding" "50px 0" ] [ proj model ]
                , header
                ]
            , Markdown.toHtml
                [ style "margin" "0 auto"
                , style "width" "70vw"
                , style "max-width" "800px"
                , style "padding-bottom" "50px"
                ]
                description
            ]
        ]
    , title = Array.get (day - 1) titles |> Maybe.withDefault ""
    }


titles : Array String
titles =
    Array.fromList
        [ "Argyle"
        , "RYB"
        , "Anatoly"
        , "Frequency"
        , "Automaton"
        , "Poisson"
        , "Loops"
        , "Cradle"
        , "Nicky"
        , "Starfield"
        , "Parallax"
        , "Url Parallax"
        , "DVD"
        , "Moiré"
        , "Polyomino"
        , "Tiles"
        , "Headache"
        , "Rotating Polygon"
        , "Matrix"
        , "Gravity"
        , "Star System"
        , "Spirograph"
        , "Braids"
        , "Koalas"
        , "Boids"
        , "Jam"
        , "Areas"
        , "Borders"
        , "Waves"
        , "Lightning"
        ]
