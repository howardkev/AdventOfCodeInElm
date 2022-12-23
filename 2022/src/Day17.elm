module Day17 exposing (..)

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
import BigInt
import Day10 exposing (State)
import Array exposing (Array)

todayDescription : PuzzleDescription
todayDescription = { day = 17, title = "Pyroclastic Flow" }

sampleInput : String
sampleInput =
    String.trim """
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
"""

--    """
--####
--
--.#.
--###
--.#.
--
--..#
--..#
--###
--
--#
--#
--#
--#
--
--##
--##
--"""

type Direction = Left | Right
type alias Coordinate = (Int, Int)
type Move = Push | Fall
type alias Block = Set Coordinate
type alias Board = Set Coordinate
type alias Commands = Array Direction

type alias State = 
    { currentRockId : Int
    , grid : Board
    , position : Coordinate
    , stopped : Int
    , instructionId : Int
    , top : Int 
    }

rocks : Dict Int (Int, Int, Block)
rocks = 
    Dict.fromList 
        [ (0, (4, 1, Set.fromList [(0,0), (1,0), (2,0), (3,0)]))
        , (1, (3, 3, Set.fromList [(1,0), (0,-1), (1,-1), (2,-1), (1,-2)]))
        , (2, (3, 3, Set.fromList [(0,0), (1,0), (2,0), (2,-1), (2,-2)]))
        , (3, (1, 4, Set.fromList [(0,0), (0,-1), (0,-2), (0,-3)]))
        , (4, (2, 2, Set.fromList [(0,0), (1,0), (0,-1), (1,-1)]))
        ]

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> solve initialState
        |> .top
        |> (*) -1
        --|> printGrid

parseInput : String -> Array Direction
parseInput input =
    input
        |> String.toList
        |> filterMap toDirection
        |> Array.fromList

toDirection : Char -> Maybe Direction
toDirection c =
    case c of
        '<' -> Just Left
        '>' -> Just Right
        _ -> Nothing

initialState : State
initialState =
    { currentRockId = 0
    , grid = Set.fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0)]
    , position = (3, -4)
    , stopped = 0
    , instructionId = 0
    , top = 0
    }

solve : State -> Commands -> State
solve state commands =
    if state.stopped == 2022 then
        state
    else
        let
            instruction = case Array.get state.instructionId commands of
                Just n -> n
                _ -> Debug.todo ("should not be here:" ++ (Debug.toString state.instructionId))

            nextState = executeInstruction instruction state
            nextInstructionId = modBy (Array.length commands) (state.instructionId + 1)
        in
        solve
            { nextState 
                | instructionId = nextInstructionId 
            } 
            commands

executeInstruction : Direction -> State -> State
executeInstruction instruction state =
    let
        (w,_,rock) = Dict.get state.currentRockId rocks 
            |> Maybe.withDefault (0,0,Set.empty)

        canMoveSide (x,y) =
            let
                movedRock = Set.map (\(dx,dy) -> (x + dx, y + dy)) rock
            in
            Set.size (Set.intersect movedRock state.grid) == 0

        moveSideToSide (x, y) = 
            case instruction of
                Left ->
                    let
                        checkPos = (max (x - 1) 1, y)
                    in
                    if canMoveSide checkPos then
                        checkPos
                    else
                        (x, y)
                Right ->
                    let
                        checkPos = (min (x + 1) (8 - w), y)
                    in
                    if canMoveSide checkPos then
                        checkPos
                    else
                        (x, y)

        moveDown (x, y) = (x, y + 1)

        overlaps (x, y) =
            let
                movedRock = Set.map (\(dx,dy) -> (x + dx, y + dy)) rock
            in
            Set.size (Set.intersect movedRock state.grid) > 0

        sidePosition = moveSideToSide state.position
        canMoveDown = not (overlaps (moveDown sidePosition))
        position = 
            if canMoveDown then
                moveDown sidePosition
            else
                sidePosition
        newRocks = 
            if canMoveDown then
                Set.empty
            else
                Set.map (\(dx,dy) -> (Tuple.first position + dx, Tuple.second position + dy)) rock
        newGrid = Set.union state.grid newRocks
        top = foldl (\((_, y)) b -> min y b) 1000000 (Set.toList newGrid)
    in
    { state 
        | position = if canMoveDown then position else (3, top - 4)
        , currentRockId = if canMoveDown then state.currentRockId else (modBy 5 (state.currentRockId + 1))
        , grid = newGrid
        , stopped = if canMoveDown then state.stopped else state.stopped + 1
        , top = top
    }

printGrid : State -> String
printGrid board =
    let
        (_,_,rock) = Dict.get board.currentRockId rocks 
            |> Maybe.withDefault (0,0,Set.empty)
        movedRock = Set.map (\(x,y) -> (x + Tuple.first board.position, y + Tuple.second board.position)) rock

        grid = board.grid
        asList = Set.toList grid
        top = foldl (\((x,y)) b -> min y b) 1000000 asList - 10
        bottom = foldl (\((x,y)) b -> max y b) 0 asList
        right = 8
        left = 0
        points = pointGrid left top (right - left + 1) (bottom - top + 1)
    in
    displayBoard points grid (right - left + 1) movedRock

displayBoard : List Coordinate -> Board -> Int -> Block -> String
displayBoard points board w rock =
    let
        unitToChar v =
            if v == (0,0) then
                '+'
            else if v == (8,0) then
                '+'
            else if Tuple.first v == 0 || Tuple.first v == 8 then
                '|'
            else if Tuple.second v == 0 then
                '-'
            else if Set.member v board then
                '#'
            else if Set.member v rock then
                '@'
            else
                '.'
    in
    foldl (\v a -> (unitToChar v) :: a) [] points
        |> List.reverse
        |> LE.groupsOf w
        |> map String.fromList
        |> String.join "\n"

--------------------------------------------------------------

part2 : a -> String
part2 _ =
    BigInt.divmod (BigInt.fromInt (1000000000000 - 1748)) (BigInt.fromInt 1750)
        |> Maybe.withDefault (BigInt.fromInt 0,BigInt.fromInt 0)
        --|> asString
        |> Tuple.first
        |> BigInt.mul (BigInt.fromInt 2796)
        |> BigInt.add (BigInt.fromInt 2798)
        |> BigInt.add (BigInt.fromInt 1178)
        |> BigInt.toString
    --BigInt.divmod (BigInt.fromInt (1000000000000 - 22)) (BigInt.fromInt 35)
    --    |> Maybe.withDefault (BigInt.fromInt 0,BigInt.fromInt 0)
    --    |> asString
        --|> Tuple.first
        --|> BigInt.mul (BigInt.fromInt 53)
        --|> BigInt.toString

-- 40
-- 10091

--("571428570","752")
--1597714284518 + need to add the 752

-- 1597714285696 too low
-- 1597714285698 !!!!!!
-- 1597714285710 high

-- 6772 - 5594 = 1178 

-- stopped, top
--(10091,1748,2798)
--(20182,1750,5594)
--(30273,1750,8390) + 2796
--(40364,1750,11186) + 2796
--(50455,1750,13982)
--(60546,1750,16778)
--(70637,1750,19574)
--(80728,1750,22370)
--(90819,1750,25166)
--(100910,1750,27962)
--(111001,1750,30758)
--(121092,1750,33554)
--(131183,1750,36350)
--(141274,1750,39146)

--        1514285714263
-- target 1514285714288

-- ("28571428570","28")
-- 1514285714210 + 39 = 1514285714249
-- 1514285714249 + 39 = 1514285714288

--("28571428571","15")

-- stopped, top
--(40,8,15)
--(80,6,23)
--(120,8,39)
-- +22, +39

--(160,7,51)
--(200,7,61)
--(240,7,70)
--(280,6,78) +27, +39
--(320,8,92)
-- +35, +53

--(360,7,104)
--(400,7,114)
--(440,7,123)
--(480,6,131)
--(520,8,145)
-- +35, +53

--(560,7,157)
--(600,7,167)
--(640,7,176)
--(680,6,184)
--(720,8,198)
-- +35, +53

asString : (BigInt.BigInt, BigInt.BigInt) -> (String, String)
asString (d,r) =
    (BigInt.toString d, BigInt.toString r)

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
