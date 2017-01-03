module Helper exposing (project)

import Html exposing (Html, div, h1, text, a)
import Html.Attributes exposing (style, href)
import Markdown
import Array exposing (Array)


title : Int -> Html msg
title day =
    case Array.get (day - 1) titles of
        Nothing ->
            Debug.crash "No day like the present"

        Just str ->
            h1 [ style [ ( "padding", "5px 30px" ) ] ] [ text <| "Day " ++ toString day ++ ": " ++ str ]


link : Int -> Html msg
link day =
    case Array.get (day - 1) titles of
        Nothing ->
            a [] []

        Just str ->
            a [ href <| "http://fishgold.co/codecember16/Day" ++ toString day ]
                -- a [ href <| "http://localhost:8000/Day" ++ toString day ++ "/Main.elm" ]
                [ text <| "Day " ++ toString day ++ ": " ++ str ]


project : Int -> (model -> Html msg) -> model -> Html msg
project day proj model =
    let
        today =
            Array.get (day - 1) titles

        yesterday =
            Array.get (day - 2) titles

        tomorrow =
            Array.get (day) titles

        description =
            Array.get (day - 1) descriptions |> Maybe.withDefault ""

        header =
            div
                [ style
                    [ ( "display", "flex" )
                    , ( "flex-direction", "row" )
                    , ( "align-items", "baseline" )
                    , ( "justify-content", "center" )
                    ]
                ]
                [ link (day - 1), title day, link (day + 1) ]
    in
        div
            []
            [ div
                [ style
                    [ ( "display", "flex" )
                    , ( "flex-direction", "column" )
                    , ( "align-items", "center" )
                    ]
                ]
                [ div [ style [ ( "padding", "50px 0" ) ] ] [ proj model ]
                , header
                ]
            , Markdown.toHtml [ style [ ( "padding", "0 50px" ) ] ] description
            ]


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


descriptions : Array String
descriptions =
    Array.fromList
        []
