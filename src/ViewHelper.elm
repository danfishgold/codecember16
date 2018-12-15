module ViewHelper exposing (centeredDiv, contentDiv, header, images, link, stringFromDay, titles)

import Array exposing (Array)
import Day01.Main as D01
import Day02.Main as D02
import Day03.Main as D03
import Day04.Main as D04
import Day05.Main as D05
import Day06.Main as D06
import Day07.Main as D07
import Day08.Main as D08
import Day09.Main as D09
import Day10.Main as D10
import Day11.Main as D11
import Day12.Main as D12
import Day13.Main as D13
import Day14.Main as D14
import Day15.Main as D15
import Day16.Main as D16
import Day17.Main as D17
import Day18.Main as D18
import Day19.Main as D19
import Day20.Main as D20
import Day21.Main as D21
import Day22.Main as D22
import Day23.Main as D23
import Day24.Main as D24
import Day25.Main as D25
import Day26.Main as D26
import Day27.Main as D27
import Day28.Main as D28
import Day29.Main as D29
import Day30.Main as D30
import Html exposing (Html, a, div, h1, h2, li, ol, text)
import Html.Attributes exposing (href, style)


centeredDiv attrs children =
    div
        ([ style "display" "flex"
         , style "flex-direction" "column"
         , style "align-items" "center"
         ]
            ++ attrs
        )
        children


contentDiv attrs children =
    div
        ([ style "margin" "0 auto"
         , style "width" "70vw"
         , style "max-width" "800px"
         , style "padding-bottom" "50px"
         ]
            ++ attrs
        )
        children


header : Html msg
header =
    h1 []
        [ a [ href "#", style "text-decoration" "none" ]
            [ text "Codecember 2016" ]
        ]


link : Bool -> Int -> Html msg
link numbered day =
    case Array.get (day - 1) titles of
        Nothing ->
            text ""

        Just title ->
            a
                [ href <| "#day" ++ stringFromDay day ]
                [ if numbered then
                    text <| "Day " ++ String.fromInt day ++ ": " ++ title

                  else
                    text title
                ]


stringFromDay : Int -> String
stringFromDay day =
    if day < 10 then
        "0" ++ String.fromInt day

    else
        String.fromInt day


titles : Array String
titles =
    Array.fromList
        [ D01.page.title
        , D02.page.title
        , D03.page.title
        , D04.page.title
        , D05.page.title
        , D06.page.title
        , D07.page.title
        , D08.page.title
        , D09.page.title
        , D10.page.title
        , D11.page.title
        , D12.page.title
        , D13.page.title
        , D14.page.title
        , D15.page.title
        , D16.page.title
        , D17.page.title
        , D18.page.title
        , D19.page.title
        , D20.page.title
        , D21.page.title
        , D22.page.title
        , D23.page.title
        , D24.page.title
        , D25.page.title
        , D26.page.title
        , D27.page.title
        , D28.page.title
        , D29.page.title
        , D30.page.title
        ]


images : Array String
images =
    Array.fromList
        [ "img/day01.svg"
        , "img/day02.svg"
        , "img/day03.svg"
        , "img/day04.png"
        , "img/day05.svg"
        , "img/day06.svg"
        , "img/day07.svg"
        , "img/day08.svg"
        , "img/day09.svg"
        , "img/day10.svg"
        , "img/day11.svg"
        , "img/day12.png"
        , "img/day13.svg"
        , "img/day14.svg"
        , "img/day15.svg"
        , "img/day16.svg"
        , "img/day17.svg"
        , "img/day18.svg"
        , "img/day19.svg"
        , "img/day20.svg"
        , "img/day21.svg"
        , "img/day22.svg"
        , "img/day23.png"
        , "img/day24.svg"
        , "img/day25.svg"
        , "img/day26.svg"
        , "img/day27.svg"
        , "img/day28.svg"
        , "img/day29.svg"
        , "img/day30.svg"
        ]
