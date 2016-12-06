module Poisson exposing (..)

import Html exposing (program)
import Html exposing (Html, div, text)
import Dict exposing (Dict)
import Set exposing (Set)
import Random exposing (Generator)
import Random.Set
import Random.Extra exposing (constant)


type alias Point =
    ( Float, Float )


type alias GridPoint =
    ( Int, Int )


type alias Model =
    { activeList : Set Point
    , grid : Dict GridPoint Point
    , finalPoints : List Point
    , width : Float
    , height : Float
    , k : Int
    , r : Float
    }


type Msg
    = MoveToFinal Point
    | AddActive Point
    | Finished


init : Float -> Float -> Int -> Float -> ( Model, Cmd Msg )
init width height k r =
    ( { activeList = Set.empty
      , grid = Dict.empty
      , finalPoints = []
      , width = width
      , height = height
      , k = k
      , r = r
      }
    , Cmd.none
    )



{-
   choose a point p at random.
   try k random points around it
   if all those points q are near another point, remove p from active list and add it to finalPoints.
   if some point q is near an existing point in grid, add q to the active list.
   repeat until activePoints is empty
-}
--


pointAround : Float -> Point -> Generator Point
pointAround r ( x, y ) =
    let
        rad =
            Random.float 0 (3 * r * r) |> Random.map (\dr2 -> sqrt (r * r + dr2))

        arg =
            Random.float 0 (2 * pi)

        pt r t =
            ( x + r * cos t, y + r * sin t )
    in
        Random.map2 pt rad arg


dist : Point -> Point -> Float
dist ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)


isFar : Float -> Dict GridPoint Point -> Point -> Bool
isFar r grid ( x, y ) =
    let
        ( x0, y0 ) =
            ( floor x, floor y )

        keysX dy =
            [ ( x0 - 1, y0 + dy ), ( x0, y0 + dy ), ( x0 + 1, y0 + dy ) ]

        keys =
            List.range -1 2 |> List.concatMap keysX

        isFarFrom k =
            grid
                |> Dict.get k
                |> Maybe.map (dist ( x, y ))
                |> Maybe.map ((>=) r)
                |> Maybe.withDefault False
    in
        List.all isFarFrom keys


isWithin : Float -> Float -> Point -> Bool
isWithin width height ( x, y ) =
    0 <= x && x <= width && 0 <= y && y <= height


step : Model -> Generator Msg
step { activeList, k, r, grid, width, height } =
    let
        processPoint pnt =
            case pnt of
                Nothing ->
                    constant Finished

                Just p ->
                    stepFor p k

        stepFor p kp =
            let
                dealWith q =
                    if isFar r grid q && isWithin width height q then
                        constant (AddActive q)
                    else
                        stepFor p (kp - 1)
            in
                if kp == 0 then
                    constant (MoveToFinal p)
                else
                    pointAround r p
                        |> Random.andThen dealWith
    in
        Random.Set.sample activeList |> Random.andThen processPoint


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveToFinal p ->
            ( { model
                | activeList = model.activeList |> Set.remove p
                , finalPoints = p :: model.finalPoints
              }
            , Cmd.none
            )

        AddActive q ->
            ( { model | activeList = model.activeList |> Set.insert q }, Cmd.none )

        Finished ->
            ( model, Cmd.none )



--


view : Model -> Html Msg
view model =
    div [] []



--


main : Program Never Model Msg
main =
    program
        { init = init 750 500 30 10
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
