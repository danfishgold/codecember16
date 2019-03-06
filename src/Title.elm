module Title exposing (view)

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, id)


s =
    Html.Attributes.style


view =
    div [ s "height" "auto", s "width" "auto" ]
        [ a
            [ href "#"
            , id "header"
            ]
            [ span [ id "co", class "letters" ]
                [ text "CO" ]
            , span [ id "de", class "letters" ]
                [ text "DE" ]
            , span [ id "cember", class "letters" ]
                [ text "CEMBER" ]
            , div [ id "b1", class "bg" ]
                []
            , div [ id "b2", class "bg" ]
                []
            ]
        ]


between v1 v2 =
    String.fromInt v1 ++ " / " ++ String.fromInt v2


textProps col1 col2 =
    [ s "grid-column" <| between col1 col2
    , s "grid-row" <| between 1 2
    , s "padding" "0"
    , s "margin" "0"
    , s "font-family" "sans-serif"
    , s "font-weight" "bold"
    ]


bgProps color dx dy col1 col2 =
    [ s "grid-column" <| between col1 col2
    , s "grid-row" <| between 1 2
    , s "background" color
    , s "position" "relative"
    , s "top" <| String.fromFloat dy ++ "px"
    , s "left" <| String.fromFloat dx ++ "px"
    , s "width" "100%"
    , s "height" "100%"
    , s "z-index" "-1"
    ]
