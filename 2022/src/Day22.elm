module Day22 exposing (..)

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
import Html.Attributes exposing (list)
import Html exposing (a)
import Parser exposing (int)
import BigInt exposing (BigInt)
import Debug as D
import GridExample exposing (MapCell(..))
import Day2 exposing (score)

todayDescription : PuzzleDescription
todayDescription = { day = 22, title = "---" }

sampleInput : String
sampleInput =
    """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"""

type Command
    = Right
    | Left
    | Move Int

type Direction
    = North
    | East
    | South
    | West

type Unit
    = Wall
    | Floor
    | Empty

part1 input =
    input
        |> parseInput
        |> Maybe.map solve
        |> Maybe.map score

score ((x,y), direction) =
    1000 * (y + 1) + 4 * (x + 1) + case direction of
        North -> 3
        East -> 0
        South -> 1
        West -> 2

solve state =
    let
        getWithWrap (x, y) (dx, dy) wrapPoint =
            case Dict.get (x + dx, y + dy) state.grid of
                Just Wall -> (x + dx, y + dy)
                Just Floor -> (x + dx, y + dy)
                _ -> wrapPoint

        getNextPos (x, y) (dx, dy) wrapPoint =
            let
                next = getWithWrap (x, y) (dx, dy) wrapPoint
            in
            case Dict.get next state.grid of
                Just Wall -> (x, y)
                Just Floor -> next
                _ -> (x, y) --Debug.todo "bad wrap"

        getLeftWrapPos (x, y) =
            let
                (x1, _) = state.grid
                    |> Dict.toList
                    |> filter (\((_,y1),unit) -> 
                            case unit of
                                Empty -> False
                                _ -> y1 == y
                        )
                    |> map Tuple.first
                    |> head |> Maybe.withDefault (0,0)
            in
            (x1, y)

        getRightWrapPos (x, y) =
            let
                (x1, _) = state.grid
                    |> Dict.toList
                    |> filter (\((_,y1),unit) -> 
                            case unit of
                                Empty -> False
                                _ -> y1 == y
                        )
                    |> map Tuple.first
                    |> List.sortWith descending
                    |> head |> Maybe.withDefault (0,0)
            in
            (x1, y)

        getTopWrapPos (x, y) =
            let
                (_,y1) = state.grid
                    |> Dict.toList
                    |> filter (\((x1,_),unit) -> 
                            case unit of
                                Empty -> False
                                _ -> x1 == x
                        )
                    |> map Tuple.first
                    |> head |> Maybe.withDefault (0,0)
            in
            (x, y1)

        getBottomWrapPos (x, y) =
            let
                (_,y1) = state.grid
                    |> Dict.toList
                    |> filter (\((x1,_),unit) -> 
                            case unit of
                                Empty -> False
                                _ -> x1 == x
                        )
                    |> map Tuple.first
                    |> List.sortWith descending
                    |> head |> Maybe.withDefault (0,0)
            in
            (x, y1)

        move direction pos =
            case direction of
                East -> getNextPos pos (1, 0) (getLeftWrapPos pos)
                West -> getNextPos pos (-1, 0) (getRightWrapPos pos)
                North -> getNextPos pos (0, -1) (getBottomWrapPos pos)
                South -> getNextPos pos (0, 1) (getTopWrapPos pos)

        turnRight direction =
            case direction of
                North -> East
                East -> South
                South -> West
                West -> North

        turnLeft direction =
            case direction of
                North -> West
                East -> North
                South -> East
                West -> South

        oneStep command (pos, direction) =
            case command of
                Move n -> (runNTimes n (move direction) pos, direction)
                Right -> (pos, turnRight direction)
                Left -> (pos, turnLeft direction)

        startingPoint = state.grid
            |> Dict.toList
            |> LE.find (\((_,y), unit) ->
                    case unit of
                        Empty -> False
                        _ -> y == 0
                )
            |> Maybe.withDefault ((0, 0), Floor)
            |> Tuple.first
            |> D.log "start"
    in
    foldl oneStep (startingPoint, East) state.commands

parseInput input =
    input
        |> String.split "\n\n"
        |> parseBoth

parseBoth groups =
    case groups of
        [grid_, commands_] ->
            let
                numbers = commands_ 
                    |> ints
                    |> map (\n -> Move n)

                letters = commands_ 
                    |> randl
                    |> filterMap (\c ->
                            case c of
                                "R" -> Just Right
                                "L" -> Just Left
                                _ -> Nothing
                        )

                commands = LE.interweave numbers letters

                rows = grid_
                    |> String.split "\n"
                    |> map (String.split "")

                toUnit a =
                    case a of
                        "." -> Floor
                        "#" -> Wall
                        _ -> Empty

                grid = List.indexedMap (\y line -> List.indexedMap (\x value -> ((x, y), toUnit value)) line) rows
                    |> List.concat
                    |> Dict.fromList
            in
            Just { grid = grid, commands = commands }
        _ -> Nothing

randl input =
  let 
    regex = Maybe.withDefault Regex.never <|
      Regex.fromString "[R|L]+"
    found = Regex.find regex
  in
    List.map .match (found input)

--------------------------------------------------------------

part2 input =
    input
        |> parseInput
        |> Maybe.map solve2
        |> Maybe.map score

getBlockNumber (x, y) =
    if y < 50 then
        if x < 100 then
            1
        else
            2
    else if y < 100 then
        3
    else if y < 150 then
        if x < 50 then
            4
        else
            5
    else
        6

solve2 state =
    let
        getWithWrap dir (x, y) (dx, dy) wrapPoint =
            case Dict.get (x + dx, y + dy) state.grid of
                Just Wall -> ((x + dx, y + dy), dir)
                Just Floor -> ((x + dx, y + dy), dir)
                _ -> wrapPoint

        getNextPos dir (x, y) (dx, dy) wrapPoint =
            let
                (next, nextDir) = getWithWrap dir (x, y) (dx, dy) wrapPoint
            in
            case Dict.get next state.grid of
                Just Wall -> ((x, y), dir)
                Just Floor -> (next, nextDir)
                _ -> ((x, y), dir) --Debug.todo "bad wrap"

        getRightWrapPos (x, y) =
            let
                block = getBlockNumber (x,y)
            in
            if block == 2 || block == 5 then
                ((x,y), West)
            else if block == 1 then
                ((0, 149 - y), East)
            else if block == 3 then
                ((y - 50, 100), South)
            else if block == 4 then
                ((50, 49 - (y - 100)), East)
            else -- block 6
                ((50 + (y - 150), 0), South)

        getLeftWrapPos (x, y) =
            let
                block = getBlockNumber (x,y)
            in
            if block == 1 || block == 4 then
                ((x,y), East)
            else if block == 2 then
                ((99, 149 - y), West)
            else if block == 3 then
                ((100 + (y - 50), 49), North)
            else if block == 5 then
                ((149, 49 - (y - 100)), West)
            else -- block 6
                ((50 + (y - 150), 149), North)

        getTopWrapPos (x, y) =
            let
                block = getBlockNumber (x,y)
            in
            if block == 1 || block == 3 || block == 4 then
                ((x,y), South)
            else if block == 2 then
                ((99, 50 + (x - 100)), West)
            else if block == 5 then
                ((49, 150 + (x - 50)), West)
            else -- block 6
                ((100 + x, 0), South)

        getBottomWrapPos (x, y) =
            let
                block = getBlockNumber (x,y)
            in
            if block == 3 || block == 5 || block == 6 then
                ((x,y), North)
            else if block == 1 then
                ((0, 150 + (x - 50)), East)
            else if block == 2 then
                (((x - 100), 199), North)
            else -- block 4
                ((50, 50 + x), East)

        move (pos, direction) =
            case direction of
                East -> getNextPos direction pos (1, 0) (getLeftWrapPos pos)
                West -> getNextPos direction pos (-1, 0) (getRightWrapPos pos)
                North -> getNextPos direction pos (0, -1) (getBottomWrapPos pos)
                South -> getNextPos direction pos (0, 1) (getTopWrapPos pos)

        turnRight direction =
            case direction of
                North -> East
                East -> South
                South -> West
                West -> North

        turnLeft direction =
            case direction of
                North -> West
                East -> North
                South -> East
                West -> South

        oneStep command (pos, direction) =
            case command of
                Move n -> runNTimes n move (pos, direction)
                Right -> (pos, turnRight direction)
                Left -> (pos, turnLeft direction)

        startingPoint = state.grid
            |> Dict.toList
            |> LE.find (\((_,y), unit) ->
                    case unit of
                        Empty -> False
                        _ -> y == 0
                )
            |> Maybe.withDefault ((0, 0), Floor)
            |> Tuple.first
    in
    foldl oneStep (startingPoint, East) state.commands

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
