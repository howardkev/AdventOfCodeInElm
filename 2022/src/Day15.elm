module Day15 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import Set exposing (Set)
import List exposing (map, filterMap, foldl, take, filter, concat, length, range, sort, maximum, sortWith, head, tail, any, all, reverse)
import String exposing (lines, split, toList)
import Dict exposing (empty)
import Fifo exposing (Fifo)
import Parser exposing (Parser, Trailing(..))
import Day10 exposing (score)
import Day13 exposing (score2)

todayDescription : PuzzleDescription
todayDescription = { day = 15, title = "Beacon Exclusion Zone" }

sampleInput : String
sampleInput =
    String.trim """
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
"""

type alias Coordinate = (Int, Int)
type Unit 
    = Sensor Int
    | Beacon 

type alias Grid = Dict Coordinate Unit

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> solveRow
        |> score

score : List (Bool, Bool) -> Int
score results =
    LE.count (\(inRange, isBeacon) -> 
         inRange && not isBeacon) results

parseInput : String -> Grid
parseInput input =
    input
        |> String.split "\n"
        |> map ints
        |> filterMap toCoords
        |> toGrid

toCoords : List Int -> Maybe (Coordinate, Coordinate)
toCoords line =
    case line of
        [a,b,c,d] -> Just ((a,b), (c,d))
        _ -> Nothing

toGrid : List (Coordinate, Coordinate) -> Grid
toGrid points =
    foldl (\(sensor, beacon) dict ->
        let
            manhattan = manhattanDistance sensor beacon
            addedSensor = Dict.insert sensor (Sensor manhattan) dict
            addedBeacon = Dict.insert beacon Beacon addedSensor
        in
        addedBeacon
    ) Dict.empty points

manhattanDistance : Coordinate -> Coordinate -> Int
manhattanDistance (x1, y1) (x2, y2) =
    abs (x2 - x1) + abs (y2 - y1)

solveRow : Grid -> List (Bool, Bool)
solveRow grid =
    let
        row = if Dict.size grid <= 20 then 10 else 2000000
        margin = if row == 10 then 10 else 1000000
        asList = Dict.toList grid
        left = foldl (\((x,y),_) b -> min x b) 10000000 asList - margin
        right = foldl (\((x,y),_) b -> max x b) 0 asList + margin
        toCheck = range left right
        sensors = Dict.filter (\k v -> 
            case v of
                Sensor _ -> True
                _ -> False) grid
    in
    map (\x -> 
            case (checkOne grid (x, row) sensors) of
                Just n -> (True, Dict.member (x, row) grid)
                _ -> (False, False)
            ) toCheck

checkOne : a -> Coordinate -> Grid -> Maybe (Bool, Coordinate, Int)
checkOne grid pos sensors =
    let
        result = map (\sensor ->
            let
                myDistance = case sensor of
                    (p, Sensor n) -> 
                        let
                            dist = manhattanDistance pos p
                        in
                        (dist <= n, p, n)
                    _ -> Debug.todo "error"
            in
            myDistance) (Dict.toList sensors)
    in
    LE.find (\(a,_,_) -> a) result

--------------------------------------------------------------

part2 : String -> Int
part2 input =
   input
        |> parseInput
        |> solve (0,0)
        |> score2

solve : Coordinate -> Grid -> Coordinate
solve pos grid =
    let
        maxX = if Dict.size grid <= 20 then 20 else 4000000
        next = nextPos grid pos maxX
    in
    case next of
        Nothing -> pos
        Just n -> solve n grid

nextPos : Grid -> Coordinate -> Int -> Maybe Coordinate
nextPos grid (x, y) maxX =
    let
        sensors = Dict.filter (\k v -> 
            case v of
                Sensor _ -> True
                _ -> False) grid
    in
    case checkOne grid (x, y) sensors of
        Just (_, loc, man) ->
            let
                ydist = abs (Tuple.second loc - y)
                newX = Tuple.first loc + man - ydist + 1
            in
            if newX > maxX then
                Just (0, y + 1)
            else
                Just (newX, y)
        _ -> Nothing

score2 : Coordinate -> Int
score2 (x, y) =
     4000000 * x + y

-------------------------------------------------

init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = sampleInput
        , puzzleInput = Nothing
        , result = Nothing
        , description = todayDescription }, Cmd.none )

toString : String -> String
toString s =
    if String.startsWith "\"" s then
        String.dropLeft 1 s |> String.dropRight 1
    else
        s

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Puzzle1 -> (
            { model | result = Just (toString (Debug.toString (part1 model.input))) }
            , Cmd.none )
        Puzzle2 -> (
            { model | result = Just (toString (Debug.toString (part2 model.input))) }
            , Cmd.none )
        _ -> mainUpdate msg model sampleInput
    
main : Program () Model Msg
main =
  Browser.element 
    { init = init
    , update = update 
    , view = view 
    , subscriptions = subscriptions }
