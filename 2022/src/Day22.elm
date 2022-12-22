module Day22 exposing (..)

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

todayDescription : PuzzleDescription
todayDescription = { day = 22, title = "Monkey Map" }

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

type alias Coordinate = (Int, Int)
type alias BlockId = Int
type alias Location = (Coordinate, Direction)

type Command
    = TurnRight
    | TurnLeft
    | Move Int

type Direction
    = North
    | East
    | South
    | West

type Tile
    = Wall
    | Floor
    | Empty

type Sides
    = Left
    | Top
    | Right
    | Bottom

type alias Grid = Dict Coordinate Tile

type alias State = 
    { grid: Grid
    , commands: List Command
    }

part1 : String -> Maybe Int
part1 input =
    input
        |> parseInput
        |> Maybe.map solve
        |> Maybe.map score

score : Location -> Int
score ((x,y), direction) =
    1000 * (y + 1) + 4 * (x + 1) + case direction of
        North -> 3
        East -> 0
        South -> 1
        West -> 2

parseInput : String -> Maybe State
parseInput input =
    input
        |> String.split "\n\n"
        |> parseBoth

parseBoth : List String -> Maybe State
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
                                "R" -> Just TurnRight
                                "L" -> Just TurnLeft
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

randl : String -> List String
randl input =
  let 
    regex = Maybe.withDefault Regex.never <|
      Regex.fromString "[R|L]+"
    found = Regex.find regex
  in
    List.map .match (found input)

solve : State -> Location
solve state =
    let
        maxX = getMaxX state.grid
        maxY = getMaxY state.grid

        stepToValidTile (x, y) (dx, dy) =
            case Dict.get (x, y) state.grid of
                Just Wall -> (x, y)
                Just Floor -> (x, y)
                _ -> stepToValidTile (x + dx, y + dy) (dx, dy)

        getWrapPoint (x, y) (dx, dy) wrapSide =
            let
                startPoint = 
                    case wrapSide of
                        Left -> (0, y)
                        Top -> (x, 0)
                        Right -> (maxX, y)
                        Bottom -> (x, maxY)
            in
            stepToValidTile startPoint (dx, dy)

        getNextPos (x, y) (dx, dy) wrapSide =
            case Dict.get (x + dx, y + dy) state.grid of
                Just Wall -> (x, y)
                Just Floor -> (x + dx, y + dy)
                _ ->
                    let
                        potentialPoint = getWrapPoint (x, y) (dx, dy) wrapSide
                    in
                    case Dict.get potentialPoint state.grid of
                        Just Wall -> (x, y)
                        _ -> potentialPoint

        move direction pos =
            case direction of
                East -> getNextPos pos (1, 0) Left
                West -> getNextPos pos (-1, 0) Right
                North -> getNextPos pos (0, -1) Bottom
                South -> getNextPos pos (0, 1) Top

        oneStep command (pos, direction) =
            case command of
                Move n -> (runNTimes n (move direction) pos, direction)
                TurnRight -> (pos, turnRight direction)
                TurnLeft -> (pos, turnLeft direction)
    in
    foldl oneStep (getStartingPoint state.grid, East) state.commands

turnRight : Direction -> Direction
turnRight direction =
    case direction of
        North -> East
        East -> South
        South -> West
        West -> North

turnLeft : Direction -> Direction
turnLeft direction =
    case direction of
        North -> West
        East -> North
        South -> East
        West -> South

getStartingPoint : Grid -> Coordinate
getStartingPoint grid = 
    grid
        |> Dict.toList
        |> LE.find (\((_,y), unit) ->
                case unit of
                    Empty -> False
                    _ -> y == 0
            )
        |> Maybe.withDefault ((0, 0), Floor)
        |> Tuple.first

getMaxX : Grid -> Int
getMaxX grid =
    Dict.keys grid 
        |> foldl (\(x,_) acc -> max acc x) 0

getMaxY : Grid -> Int
getMaxY grid =
    Dict.keys grid 
        |> foldl (\(_,y) acc -> max acc y) 0

--------------------------------------------------------------

part2 : String -> Maybe Int
part2 input =
    input
        |> parseInput
        |> Maybe.map solve2
        |> Maybe.map score

getBlockNumber : Coordinate -> BlockId
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

solve2 : State -> Location
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

        oneStep command (pos, direction) =
            case command of
                Move n -> runNTimes n move (pos, direction)
                TurnRight -> (pos, turnRight direction)
                TurnLeft -> (pos, turnLeft direction)
    in
    foldl oneStep (getStartingPoint state.grid, East) state.commands

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
