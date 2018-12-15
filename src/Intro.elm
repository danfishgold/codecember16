module Intro exposing (view)

import Array
import Browser
import Html exposing (a, div, img, li, ol, text)
import Html.Attributes exposing (style)
import Markdown
import ViewHelper exposing (centeredDiv, contentDiv, header, link, titles)


view : Browser.Document msg
view =
    let
        projectItem idx =
            li [] [ link False idx ]
    in
    { body =
        [ div [ style "font-family" "sans-serif" ]
            [ centeredDiv [] [ header ]
            , contentDiv []
                [ Markdown.toHtml []
                    "Here are thirty projects I did during December 2016. I think they're cool."
                , titles
                    |> Array.toList
                    |> List.indexedMap (\idx _ -> projectItem (idx + 1))
                    |> ol []
                ]
            ]
        ]
    , title = "Codecember 2016"
    }
