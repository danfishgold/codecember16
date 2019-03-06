module ViewHelper exposing (Shade(..), centeredDiv, contentDiv, images, link, stringFromDay, title, url)

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


url : Int -> String
url day =
    "#day" ++ stringFromDay day


title : Bool -> Int -> Maybe String
title numbered day =
    case Array.get (day - 1) titles of
        Just ttl ->
            if numbered then
                Just <| "Day " ++ String.fromInt day ++ ": " ++ ttl

            else
                Just ttl

        Nothing ->
            Nothing


link : Bool -> Int -> Maybe (Html msg)
link numbered day =
    case title numbered day of
        Nothing ->
            Nothing

        Just ttl ->
            Just <| a [ href <| url day ] [ text ttl ]


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


type Shade
    = Dark
    | Light


images : Array ( String, Shade )
images =
    Array.fromList
        [ ( "img/day01.svg", Light )
        , ( "img/day02.svg", Light )
        , ( "img/day03.svg", Dark )
        , ( "img/day04.png", Light )
        , ( "img/day05.png", Light )
        , ( "img/day06.svg", Light )
        , ( "img/day07.svg", Light )
        , ( "img/day08.svg", Light )
        , ( "img/day09.svg", Dark )
        , ( "img/day10.svg", Dark )
        , ( "img/day11.svg", Light )
        , ( "img/day12.png", Light )
        , ( "img/day13.svg", Dark )
        , ( "img/day14.svg", Light )
        , ( "img/day15.svg", Light )
        , ( "img/day16.svg", Light )
        , ( "img/day17.svg", Light )
        , ( "img/day18.svg", Light )
        , ( "img/day19.svg", Dark )
        , ( "img/day20.svg", Light )
        , ( "img/day21.svg", Light )
        , ( "img/day22.svg", Light )
        , ( "img/day23.png", Light )
        , ( "img/day24.svg", Light )
        , ( "img/day25.svg", Light )
        , ( "img/day26.svg", Light )
        , ( "img/day27.svg", Light )
        , ( "img/day28.svg", Light )
        , ( "img/day29.svg", Light )
        , ( "img/day30.svg", Dark )
        ]
