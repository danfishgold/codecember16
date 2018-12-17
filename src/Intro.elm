module Intro exposing (view)

import Array
import Browser
import Html exposing (Html, a, div, img, text)
import Html.Attributes exposing (class, href, id, src, style)
import Markdown
import ViewHelper exposing (Shade(..), centeredDiv, contentDiv, header, images, link, title, url)


view : Browser.Document msg
view =
    { body =
        [ div [ style "font-family" "sans-serif" ]
            [ centeredDiv [] [ header ]
            , contentDiv []
                [ Markdown.toHtml []
                    "Here are thirty projects I did during December 2016. I think they're cool."
                , projectGrid
                ]
            ]
        ]
    , title = "Codecember 2016"
    }


projectGrid : Html msg
projectGrid =
    images
        |> Array.toList
        |> List.indexedMap projectBox
        |> div
            [ class "project-grid"
            ]


shadeClass : Shade -> Html.Attribute msg
shadeClass shade =
    case shade of
        Dark ->
            class "dark"

        Light ->
            class "light"


projectBox : Int -> ( String, Shade ) -> Html msg
projectBox idx ( image, shade ) =
    a
        [ href <| url (idx + 1)
        , class "project-box"
        , shadeClass shade
        ]
        [ img [ src image ] []
        , div []
            [ text <| Maybe.withDefault "" <| title True (idx + 1)
            ]
        ]
