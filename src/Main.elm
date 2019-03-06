module Main exposing (main)

import Array exposing (Array)
import Browser exposing (application)
import Browser.Dom
import Browser.Navigation
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
import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (style)
import Intro
import Markdown
import Task
import Title
import Url exposing (Url)
import ViewHelper exposing (centeredDiv, contentDiv, link, title)


type Msg
    = NoOp
    | UrlChange Url
    | UrlRequest Browser.UrlRequest
    | Msg01 D01.Msg
    | Msg02 D02.Msg
    | Msg03 D03.Msg
    | Msg04 D04.Msg
    | Msg05 D05.Msg
    | Msg06 D06.Msg
    | Msg07 D07.Msg
    | Msg08 D08.Msg
    | Msg09 D09.Msg
    | Msg10 D10.Msg
    | Msg11 D11.Msg
    | Msg12 D12.Msg
    | Msg13 D13.Msg
    | Msg14 D14.Msg
    | Msg15 D15.Msg
    | Msg16 D16.Msg
    | Msg17 D17.Msg
    | Msg18 D18.Msg
    | Msg19 D19.Msg
    | Msg20 D20.Msg
    | Msg21 D21.Msg
    | Msg22 D22.Msg
    | Msg23 D23.Msg
    | Msg24 D24.Msg
    | Msg25 D25.Msg
    | Msg26 D26.Msg
    | Msg27 D27.Msg
    | Msg28 D28.Msg
    | Msg29 D29.Msg
    | Msg30 D30.Msg


type ProjectModel
    = Model01 D01.Model
    | Model02 D02.Model
    | Model03 D03.Model
    | Model04 D04.Model
    | Model05 D05.Model
    | Model06 D06.Model
    | Model07 D07.Model
    | Model08 D08.Model
    | Model09 D09.Model
    | Model10 D10.Model
    | Model11 D11.Model
    | Model12 D12.Model
    | Model13 D13.Model
    | Model14 D14.Model
    | Model15 D15.Model
    | Model16 D16.Model
    | Model17 D17.Model
    | Model18 D18.Model
    | Model19 D19.Model
    | Model20 D20.Model
    | Model21 D21.Model
    | Model22 D22.Model
    | Model23 D23.Model
    | Model24 D24.Model
    | Model25 D25.Model
    | Model26 D26.Model
    | Model27 D27.Model
    | Model28 D28.Model
    | Model29 D29.Model
    | Model30 D30.Model


type Page project intro
    = Project project
    | Intro intro


type alias Model =
    { page : Page ProjectModel {}
    , key : Browser.Navigation.Key
    }


type ProjectUrl
    = Url01
    | Url02
    | Url03
    | Url04
    | Url05
    | Url06
    | Url07
    | Url08
    | Url09
    | Url10
    | Url11
    | Url12
    | Url13
    | Url14
    | Url15
    | Url16
    | Url17
    | Url18
    | Url19
    | Url20
    | Url21
    | Url22
    | Url23
    | Url24
    | Url25
    | Url26
    | Url27
    | Url28
    | Url29
    | Url30


parseUrl : Url -> Page ProjectUrl ()
parseUrl url =
    case url.fragment |> Maybe.withDefault "" of
        "day01" ->
            Project Url01

        "day02" ->
            Project Url02

        "day03" ->
            Project Url03

        "day04" ->
            Project Url04

        "day05" ->
            Project Url05

        "day06" ->
            Project Url06

        "day07" ->
            Project Url07

        "day08" ->
            Project Url08

        "day09" ->
            Project Url09

        "day10" ->
            Project Url10

        "day11" ->
            Project Url11

        "day12" ->
            Project Url12

        "day13" ->
            Project Url13

        "day14" ->
            Project Url14

        "day15" ->
            Project Url15

        "day16" ->
            Project Url16

        "day17" ->
            Project Url17

        "day18" ->
            Project Url18

        "day19" ->
            Project Url19

        "day20" ->
            Project Url20

        "day21" ->
            Project Url21

        "day22" ->
            Project Url22

        "day23" ->
            Project Url23

        "day24" ->
            Project Url24

        "day25" ->
            Project Url25

        "day26" ->
            Project Url26

        "day27" ->
            Project Url27

        "day28" ->
            Project Url28

        "day29" ->
            Project Url29

        "day30" ->
            Project Url30

        _ ->
            Intro ()


initialModelFromUrl : ProjectUrl -> ( ProjectModel, Cmd Msg )
initialModelFromUrl url =
    case url of
        Url01 ->
            mapInit Model01 Msg01 D01.page.init

        Url02 ->
            mapInit Model02 Msg02 D02.page.init

        Url03 ->
            mapInit Model03 Msg03 D03.page.init

        Url04 ->
            mapInit Model04 Msg04 D04.page.init

        Url05 ->
            mapInit Model05 Msg05 D05.page.init

        Url06 ->
            mapInit Model06 Msg06 D06.page.init

        Url07 ->
            mapInit Model07 Msg07 D07.page.init

        Url08 ->
            mapInit Model08 Msg08 D08.page.init

        Url09 ->
            mapInit Model09 Msg09 D09.page.init

        Url10 ->
            mapInit Model10 Msg10 D10.page.init

        Url11 ->
            mapInit Model11 Msg11 D11.page.init

        Url12 ->
            mapInit Model12 Msg12 D12.page.init

        Url13 ->
            mapInit Model13 Msg13 D13.page.init

        Url14 ->
            mapInit Model14 Msg14 D14.page.init

        Url15 ->
            mapInit Model15 Msg15 D15.page.init

        Url16 ->
            mapInit Model16 Msg16 D16.page.init

        Url17 ->
            mapInit Model17 Msg17 D17.page.init

        Url18 ->
            mapInit Model18 Msg18 D18.page.init

        Url19 ->
            mapInit Model19 Msg19 D19.page.init

        Url20 ->
            mapInit Model20 Msg20 D20.page.init

        Url21 ->
            mapInit Model21 Msg21 D21.page.init

        Url22 ->
            mapInit Model22 Msg22 D22.page.init

        Url23 ->
            mapInit Model23 Msg23 D23.page.init

        Url24 ->
            mapInit Model24 Msg24 D24.page.init

        Url25 ->
            mapInit Model25 Msg25 D25.page.init

        Url26 ->
            mapInit Model26 Msg26 D26.page.init

        Url27 ->
            mapInit Model27 Msg27 D27.page.init

        Url28 ->
            mapInit Model28 Msg28 D28.page.init

        Url29 ->
            mapInit Model29 Msg29 D29.page.init

        Url30 ->
            mapInit Model30 Msg30 D30.page.init


mapInit : (model -> ProjectModel) -> (msg -> Msg) -> (() -> ( model, Cmd msg )) -> ( ProjectModel, Cmd Msg )
mapInit toModel toMsg init_ =
    let
        ( projectModel_, cmd_ ) =
            init_ ()
    in
    ( toModel projectModel_, Cmd.map toMsg cmd_ )


mapUpdate : (msg -> Msg) -> (model -> Page ProjectModel {}) -> (msg -> model -> ( model, Cmd msg )) -> msg -> model -> Model -> ( Model, Cmd Msg )
mapUpdate toMsg toModel update_ msg projectModel model =
    let
        ( projectModel_, cmd_ ) =
            update_ msg projectModel
    in
    ( { model | page = toModel projectModel_ }, Cmd.map toMsg cmd_ )


mapView : (msg -> Msg) -> { a | title : String, description : String, body : model -> Html msg } -> model -> Int -> Browser.Document Msg
mapView toMsg page model idx =
    projectView idx page.description <| Html.map toMsg <| page.body model


mapSubs : (msg -> Msg) -> (model -> Sub msg) -> model -> Sub Msg
mapSubs toMsg subs_ model =
    subs_ model |> Sub.map toMsg


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    let
        projectInit toProjModel toMsg init_ =
            let
                ( projectModel, cmd ) =
                    mapInit toProjModel toMsg init_
            in
            ( { page = Project projectModel, key = key }, cmd )
    in
    case parseUrl url of
        Project Url01 ->
            projectInit Model01 Msg01 D01.page.init

        Project Url02 ->
            projectInit Model02 Msg02 D02.page.init

        Project Url03 ->
            projectInit Model03 Msg03 D03.page.init

        Project Url04 ->
            projectInit Model04 Msg04 D04.page.init

        Project Url05 ->
            projectInit Model05 Msg05 D05.page.init

        Project Url06 ->
            projectInit Model06 Msg06 D06.page.init

        Project Url07 ->
            projectInit Model07 Msg07 D07.page.init

        Project Url08 ->
            projectInit Model08 Msg08 D08.page.init

        Project Url09 ->
            projectInit Model09 Msg09 D09.page.init

        Project Url10 ->
            projectInit Model10 Msg10 D10.page.init

        Project Url11 ->
            projectInit Model11 Msg11 D11.page.init

        Project Url12 ->
            projectInit Model12 Msg12 D12.page.init

        Project Url13 ->
            projectInit Model13 Msg13 D13.page.init

        Project Url14 ->
            projectInit Model14 Msg14 D14.page.init

        Project Url15 ->
            projectInit Model15 Msg15 D15.page.init

        Project Url16 ->
            projectInit Model16 Msg16 D16.page.init

        Project Url17 ->
            projectInit Model17 Msg17 D17.page.init

        Project Url18 ->
            projectInit Model18 Msg18 D18.page.init

        Project Url19 ->
            projectInit Model19 Msg19 D19.page.init

        Project Url20 ->
            projectInit Model20 Msg20 D20.page.init

        Project Url21 ->
            projectInit Model21 Msg21 D21.page.init

        Project Url22 ->
            projectInit Model22 Msg22 D22.page.init

        Project Url23 ->
            projectInit Model23 Msg23 D23.page.init

        Project Url24 ->
            projectInit Model24 Msg24 D24.page.init

        Project Url25 ->
            projectInit Model25 Msg25 D25.page.init

        Project Url26 ->
            projectInit Model26 Msg26 D26.page.init

        Project Url27 ->
            projectInit Model27 Msg27 D27.page.init

        Project Url28 ->
            projectInit Model28 Msg28 D28.page.init

        Project Url29 ->
            projectInit Model29 Msg29 D29.page.init

        Project Url30 ->
            projectInit Model30 Msg30 D30.page.init

        Intro () ->
            ( { page = Intro {}, key = key }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( UrlChange url, _ ) ->
            ( model, Cmd.none )

        ( UrlRequest (Browser.Internal url), _ ) ->
            let
                ( newModel, cmd ) =
                    init () url model.key
            in
            ( newModel
            , Cmd.batch
                [ cmd
                , Browser.Navigation.pushUrl model.key (Url.toString url)
                , Task.perform (always NoOp) <| Browser.Dom.setViewport 0 0
                ]
            )

        ( UrlRequest (Browser.External url), _ ) ->
            ( model, Browser.Navigation.load url )

        ( Msg01 msg_, Project (Model01 model_) ) ->
            mapUpdate Msg01 (Project << Model01) D01.page.update msg_ model_ model

        ( Msg02 msg_, Project (Model02 model_) ) ->
            mapUpdate Msg02 (Project << Model02) D02.page.update msg_ model_ model

        ( Msg03 msg_, Project (Model03 model_) ) ->
            mapUpdate Msg03 (Project << Model03) D03.page.update msg_ model_ model

        ( Msg04 msg_, Project (Model04 model_) ) ->
            mapUpdate Msg04 (Project << Model04) D04.page.update msg_ model_ model

        ( Msg05 msg_, Project (Model05 model_) ) ->
            mapUpdate Msg05 (Project << Model05) D05.page.update msg_ model_ model

        ( Msg06 msg_, Project (Model06 model_) ) ->
            mapUpdate Msg06 (Project << Model06) D06.page.update msg_ model_ model

        ( Msg07 msg_, Project (Model07 model_) ) ->
            mapUpdate Msg07 (Project << Model07) D07.page.update msg_ model_ model

        ( Msg08 msg_, Project (Model08 model_) ) ->
            mapUpdate Msg08 (Project << Model08) D08.page.update msg_ model_ model

        ( Msg09 msg_, Project (Model09 model_) ) ->
            mapUpdate Msg09 (Project << Model09) D09.page.update msg_ model_ model

        ( Msg10 msg_, Project (Model10 model_) ) ->
            mapUpdate Msg10 (Project << Model10) D10.page.update msg_ model_ model

        ( Msg11 msg_, Project (Model11 model_) ) ->
            mapUpdate Msg11 (Project << Model11) D11.page.update msg_ model_ model

        ( Msg12 msg_, Project (Model12 model_) ) ->
            mapUpdate Msg12 (Project << Model12) D12.page.update msg_ model_ model

        ( Msg13 msg_, Project (Model13 model_) ) ->
            mapUpdate Msg13 (Project << Model13) D13.page.update msg_ model_ model

        ( Msg14 msg_, Project (Model14 model_) ) ->
            mapUpdate Msg14 (Project << Model14) D14.page.update msg_ model_ model

        ( Msg15 msg_, Project (Model15 model_) ) ->
            mapUpdate Msg15 (Project << Model15) D15.page.update msg_ model_ model

        ( Msg16 msg_, Project (Model16 model_) ) ->
            mapUpdate Msg16 (Project << Model16) D16.page.update msg_ model_ model

        ( Msg17 msg_, Project (Model17 model_) ) ->
            mapUpdate Msg17 (Project << Model17) D17.page.update msg_ model_ model

        ( Msg18 msg_, Project (Model18 model_) ) ->
            mapUpdate Msg18 (Project << Model18) D18.page.update msg_ model_ model

        ( Msg19 msg_, Project (Model19 model_) ) ->
            mapUpdate Msg19 (Project << Model19) D19.page.update msg_ model_ model

        ( Msg20 msg_, Project (Model20 model_) ) ->
            mapUpdate Msg20 (Project << Model20) D20.page.update msg_ model_ model

        ( Msg21 msg_, Project (Model21 model_) ) ->
            mapUpdate Msg21 (Project << Model21) D21.page.update msg_ model_ model

        ( Msg22 msg_, Project (Model22 model_) ) ->
            mapUpdate Msg22 (Project << Model22) D22.page.update msg_ model_ model

        ( Msg23 msg_, Project (Model23 model_) ) ->
            mapUpdate Msg23 (Project << Model23) D23.page.update msg_ model_ model

        ( Msg24 msg_, Project (Model24 model_) ) ->
            mapUpdate Msg24 (Project << Model24) D24.page.update msg_ model_ model

        ( Msg25 msg_, Project (Model25 model_) ) ->
            mapUpdate Msg25 (Project << Model25) D25.page.update msg_ model_ model

        ( Msg26 msg_, Project (Model26 model_) ) ->
            mapUpdate Msg26 (Project << Model26) D26.page.update msg_ model_ model

        ( Msg27 msg_, Project (Model27 model_) ) ->
            mapUpdate Msg27 (Project << Model27) D27.page.update msg_ model_ model

        ( Msg28 msg_, Project (Model28 model_) ) ->
            mapUpdate Msg28 (Project << Model28) D28.page.update msg_ model_ model

        ( Msg29 msg_, Project (Model29 model_) ) ->
            mapUpdate Msg29 (Project << Model29) D29.page.update msg_ model_ model

        ( Msg30 msg_, Project (Model30 model_) ) ->
            mapUpdate Msg30 (Project << Model30) D30.page.update msg_ model_ model

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Project (Model01 model_) ->
            mapView Msg01 D01.page model_ 1

        Project (Model02 model_) ->
            mapView Msg02 D02.page model_ 2

        Project (Model03 model_) ->
            mapView Msg03 D03.page model_ 3

        Project (Model04 model_) ->
            mapView Msg04 D04.page model_ 4

        Project (Model05 model_) ->
            mapView Msg05 D05.page model_ 5

        Project (Model06 model_) ->
            mapView Msg06 D06.page model_ 6

        Project (Model07 model_) ->
            mapView Msg07 D07.page model_ 7

        Project (Model08 model_) ->
            mapView Msg08 D08.page model_ 8

        Project (Model09 model_) ->
            mapView Msg09 D09.page model_ 9

        Project (Model10 model_) ->
            mapView Msg10 D10.page model_ 10

        Project (Model11 model_) ->
            mapView Msg11 D11.page model_ 11

        Project (Model12 model_) ->
            mapView Msg12 D12.page model_ 12

        Project (Model13 model_) ->
            mapView Msg13 D13.page model_ 13

        Project (Model14 model_) ->
            mapView Msg14 D14.page model_ 14

        Project (Model15 model_) ->
            mapView Msg15 D15.page model_ 15

        Project (Model16 model_) ->
            mapView Msg16 D16.page model_ 16

        Project (Model17 model_) ->
            mapView Msg17 D17.page model_ 17

        Project (Model18 model_) ->
            mapView Msg18 D18.page model_ 18

        Project (Model19 model_) ->
            mapView Msg19 D19.page model_ 19

        Project (Model20 model_) ->
            mapView Msg20 D20.page model_ 20

        Project (Model21 model_) ->
            mapView Msg21 D21.page model_ 21

        Project (Model22 model_) ->
            mapView Msg22 D22.page model_ 22

        Project (Model23 model_) ->
            mapView Msg23 D23.page model_ 23

        Project (Model24 model_) ->
            mapView Msg24 D24.page model_ 24

        Project (Model25 model_) ->
            mapView Msg25 D25.page model_ 25

        Project (Model26 model_) ->
            mapView Msg26 D26.page model_ 26

        Project (Model27 model_) ->
            mapView Msg27 D27.page model_ 27

        Project (Model28 model_) ->
            mapView Msg28 D28.page model_ 28

        Project (Model29 model_) ->
            mapView Msg29 D29.page model_ 29

        Project (Model30 model_) ->
            mapView Msg30 D30.page model_ 30

        Intro _ ->
            Intro.view


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Project (Model01 model_) ->
            mapSubs Msg01 D01.page.subscriptions model_

        Project (Model02 model_) ->
            mapSubs Msg02 D02.page.subscriptions model_

        Project (Model03 model_) ->
            mapSubs Msg03 D03.page.subscriptions model_

        Project (Model04 model_) ->
            mapSubs Msg04 D04.page.subscriptions model_

        Project (Model05 model_) ->
            mapSubs Msg05 D05.page.subscriptions model_

        Project (Model06 model_) ->
            mapSubs Msg06 D06.page.subscriptions model_

        Project (Model07 model_) ->
            mapSubs Msg07 D07.page.subscriptions model_

        Project (Model08 model_) ->
            mapSubs Msg08 D08.page.subscriptions model_

        Project (Model09 model_) ->
            mapSubs Msg09 D09.page.subscriptions model_

        Project (Model10 model_) ->
            mapSubs Msg10 D10.page.subscriptions model_

        Project (Model11 model_) ->
            mapSubs Msg11 D11.page.subscriptions model_

        Project (Model12 model_) ->
            mapSubs Msg12 D12.page.subscriptions model_

        Project (Model13 model_) ->
            mapSubs Msg13 D13.page.subscriptions model_

        Project (Model14 model_) ->
            mapSubs Msg14 D14.page.subscriptions model_

        Project (Model15 model_) ->
            mapSubs Msg15 D15.page.subscriptions model_

        Project (Model16 model_) ->
            mapSubs Msg16 D16.page.subscriptions model_

        Project (Model17 model_) ->
            mapSubs Msg17 D17.page.subscriptions model_

        Project (Model18 model_) ->
            mapSubs Msg18 D18.page.subscriptions model_

        Project (Model19 model_) ->
            mapSubs Msg19 D19.page.subscriptions model_

        Project (Model20 model_) ->
            mapSubs Msg20 D20.page.subscriptions model_

        Project (Model21 model_) ->
            mapSubs Msg21 D21.page.subscriptions model_

        Project (Model22 model_) ->
            mapSubs Msg22 D22.page.subscriptions model_

        Project (Model23 model_) ->
            mapSubs Msg23 D23.page.subscriptions model_

        Project (Model24 model_) ->
            mapSubs Msg24 D24.page.subscriptions model_

        Project (Model25 model_) ->
            mapSubs Msg25 D25.page.subscriptions model_

        Project (Model26 model_) ->
            mapSubs Msg26 D26.page.subscriptions model_

        Project (Model27 model_) ->
            mapSubs Msg27 D27.page.subscriptions model_

        Project (Model28 model_) ->
            mapSubs Msg28 D28.page.subscriptions model_

        Project (Model29 model_) ->
            mapSubs Msg29 D29.page.subscriptions model_

        Project (Model30 model_) ->
            mapSubs Msg30 D30.page.subscriptions model_

        Intro _ ->
            Sub.none


projectView : Int -> String -> Html Msg -> Browser.Document Msg
projectView day description project_ =
    let
        ttl =
            title True day
                |> Maybe.withDefault "No day like the present"

        projectTitle =
            div
                [ style "display" "flex"
                , style "flex-direction" "row"
                , style "align-items" "baseline"
                , style "justify-content" "center"
                ]
                [ Maybe.withDefault (text "") <| link True (day - 1)
                , h2
                    [ style "padding" "5px 30px" ]
                    [ text <| ttl
                    ]
                , Maybe.withDefault (text "") <| link True (day + 1)
                ]
    in
    { body =
        [ div [ style "font-family" "sans-serif" ]
            [ centeredDiv []
                [ Title.view
                , projectTitle
                , div [ style "padding" "0 0 50px 0" ] [ project_ ]
                ]
            , contentDiv [] [ Markdown.toHtml [] description ]
            ]
        ]
    , title = ttl
    }


main : Program () Model Msg
main =
    application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }
