module Day23 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import List exposing (map, filterMap, foldl, take, filter, concat, length, range, sort, maximum, sortWith, head, tail, any, all, reverse)
import Parser exposing (Parser, Trailing(..))
import Html exposing (a)
import Debug as D
import Day20 exposing (State)

todayDescription : PuzzleDescription
todayDescription = { day = 23, title = "Unstable Diffusion" }

sampleInput : String
sampleInput =
    """
....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
"""

type alias Coordinate = (Int, Int)
type alias Grid = Dict Coordinate Tile

type Tile
    = Elf
    | Ground

type Direction
    = North
    | South
    | East
    | West

type alias State =
    { numberMoved : Int
    , round : Int
    , grid : Grid
    , moves : List Direction 
    }

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> toInitialState
        |> runNTimes 10 oneRound
        --|> .grid
        --|> printGrid tileToChar
        |> score

parseInput : String -> Grid
parseInput input =
    input
        |> String.split "\n"
        |> map String.toList
        |> toGrid

toGrid : List (List Char) -> Grid
toGrid lines =
    let
        toTile a = 
            case a of
                '#' -> Just Elf
                _ -> Nothing

        maybeFilter (pos, unit) dest =
            case unit of
                Just n -> (pos, n) :: dest
                Nothing -> dest
    in
    lines
        |> List.indexedMap (\y line -> 
            List.indexedMap (\x value -> 
                ((x, y), toTile value)) line)
        |> List.concat
        |> List.foldr maybeFilter []
        |> Dict.fromList

tileToChar : Coordinate -> Grid -> Char
tileToChar v grid =
    case Dict.get v grid of
        Just Elf -> '#'
        _ -> '.'

toInitialState : Grid -> State
toInitialState grid =
    { grid = grid, moves = [North, South, West, East]
    , round = 0, numberMoved = 1 }

oneRound : State -> State
oneRound state =
    let
        count (x, y) positions =
            LE.count (\(dx, dy) -> 
                    case Dict.get (x + dx, y + dy) state.grid of
                        Just Elf -> True
                        _ -> False
                ) positions

        (firstPass, keep) = Dict.foldl (\((x, y) as pos) v (dict, kp) ->
            case v of
                Elf ->
                    let
                        north = count pos [(-1,-1), (0,-1), (1,-1)]
                        south = count pos [(-1,1), (0,1), (1,1)]
                        east = count pos [(1,-1), (1,0), (1,1)]
                        west = count pos [(-1,-1), (-1,0), (-1,1)]
                        total = north + south + east + west

                        results = filterMap (\move ->
                                case move of
                                    North -> if north == 0 then Just (x, y - 1) else Nothing
                                    South -> if south == 0 then Just (x, y + 1) else Nothing
                                    East -> if east == 0 then Just (x + 1, y) else Nothing
                                    West -> if west == 0 then Just (x - 1, y) else Nothing
                            ) state.moves
                            |> head
                    in
                    if total == 0 then
                        (dict, Dict.insert pos Elf kp)
                    else
                        case results of
                            Just pt -> (Dict.insert pos pt dict, kp)
                            _ -> (dict, Dict.insert pos Elf kp)
                _ -> (dict, kp)
            ) (Dict.empty, Dict.empty) state.grid

        goodMoves = Dict.values firstPass
            |> LE.frequencies
            |> filter (\(_, cnt) -> cnt == 1)
            |> map Tuple.first

        secondRound = 
            firstPass
                |> Dict.foldl (\pos destination dict -> 
                        if List.member destination goodMoves then
                            Dict.insert destination Elf dict
                        else
                            Dict.insert pos Elf dict
                    ) Dict.empty 

        newMoves = case LE.uncons state.moves of
            Just (h, tl) -> tl ++ [h]
            _ -> state.moves
    in
    { state 
        | grid = Dict.union keep secondRound
        , moves = newMoves
        , round = state.round + 1
        , numberMoved = length goodMoves
    } 

score : State -> Int
score state =
    let
        ((minX, maxX), (minY, maxY)) =
            Dict.foldl (\(x,y) v ((x1, x2), (y1, y2)) -> 
                    case v of
                        Elf -> ((min x1 x, max x2 x), (min y1 y, max y2 y))
                        _ -> ((x1, x2), (y1, y2))
                ) ((10000000000, 0), (10000000000, 0)) state.grid

        calculationArea = pointGrid minX minY (maxX - minX + 1) (maxY - minY + 1)
    in
    LE.count (\pt -> case Dict.get pt state.grid of
            Just Elf -> False
            _ -> True
        ) calculationArea

--------------------------------------------------------------

part2 : String -> Int
part2 input =
    input
        |> parseInput
        |> toInitialState
        |> runTillFinished

runTillFinished : State -> Int
runTillFinished state =
    if state.numberMoved == 0 then
        state.round
    else
        runTillFinished <| oneRound state

-------------------------------------------------

init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = String.trim sampleInput
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
