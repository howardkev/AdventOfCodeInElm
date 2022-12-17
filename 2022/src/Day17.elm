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
import Day10 exposing (State)
import Array

todayDescription : PuzzleDescription
todayDescription = { day = 17, title = "Pyroclastic Flow" }

sampleInput : String
sampleInput =
    """
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
"""
type Direction = Left | Right
type alias Coordinate = (Int, Int)
type Move = Push | Fall

rocks = 
    Dict.fromList 
        [ (0, (4, 1, [(0,0), (1,0), (2,0), (3,0)]))
        , (1, (3, 3, [(1,0), (0,-1), (1,-1), (2,-1), (1,-2)]))
        , (2, (3, 3, [(0,0), (1,0), (2,0), (2,-1), (2,-2)]))
        , (3, (1, 4, [(0,0), (0,-1), (0,-2), (0,-3)]))
        , (4, (2, 2, [(0,0), (1,0), (0,-1), (1,-1)]))
        ]
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

part1 input =
    input
        |> parseInput
        |> toInitialState
        |> executeInstructions
        |> .finalHeight
        --|> printGrid

parseInput input =
    input
        |> String.toList
        |> filterMap toDirection

toDirection c =
    case c of
        '<' -> Just Left
        '>' -> Just Right
        _ -> Nothing

toInitialState commands =
    { currentPattern = 0
    , grid = Set.fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0)]
    , bottom = 0
    , position = (3, -4)
    , stopped = 0
    , commands = Array.fromList commands
    , currentCommand = 0
    , finalHeight = 0
    }

executeInstructions state =
    if state.stopped == 2022 then
    --if state.stopped == 1000000000000 then
        let
            tall = foldl (\((x,y)) b -> min y b) 1000000 (Set.toList state.grid) 
        in
        { state | finalHeight = tall * -1 }
    else
        let
            index = modBy (Array.length state.commands) state.currentCommand
            instruction = Array.get index state.commands
                |> Maybe.withDefault Right
            nextState = executeInstruction instruction state
        in
        executeInstructions { nextState | currentCommand = index + 1 } 

executeInstruction instruction state =
    let
        (w,_,rock) = Dict.get state.currentPattern rocks |> Maybe.withDefault (0,0,[])

        canMoveSide (x,y) =
            let
                movedRock = map (\(dx,dy) -> (x + dx, y + dy)) rock
            in
            not <| (any (\pos -> Set.member pos state.grid) movedRock)

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

        moveDown (x, y) =
            (x, y + 1)

        overlaps (x, y) =
            let
                movedRock = map (\(dx,dy) -> (x + dx, y + dy)) rock
            in
            any (\pos -> Set.member pos state.grid) movedRock

        sidePosition = moveSideToSide state.position
        canMoveDown = not (overlaps (moveDown sidePosition))
        position = 
            if canMoveDown then
                moveDown sidePosition
            else
                sidePosition
        newRocks = 
            if canMoveDown then
                []
            else
                map (\(dx,dy) -> (Tuple.first position + dx, Tuple.second position + dy)) rock
        newGrid = foldl (\pos g -> Set.insert pos g) state.grid newRocks
        top = foldl (\((x,y)) b -> min y b) 1000000 (Set.toList newGrid)
            --|> Debug.log "top"
    in
    { state 
        | position = if canMoveDown then position else (3, top - 4)
        , currentPattern = if canMoveDown then state.currentPattern else (modBy 5 (state.currentPattern + 1))
        , grid = newGrid
        , stopped = if canMoveDown then state.stopped else state.stopped + 1
    }

printGrid board =
    let
        (_,_,rock) = Dict.get board.currentPattern rocks |> Maybe.withDefault (0,0,[])
            --|> Debug.log "rock"
        movedRock = map (\(x,y) -> (x + Tuple.first board.position, y + Tuple.second board.position)) rock
            --|> Debug.log "moved"

        grid = board.grid
        asList = Set.toList grid
        top = foldl (\((x,y)) b -> min y b) 1000000 asList - 10
        bottom = foldl (\((x,y)) b -> max y b) 0 asList
        right = 8
        left = 0
        points = pointGrid left top (right - left + 1) (bottom - top + 1)
    in
    displayBoard points grid (right - left + 1) movedRock

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
            else if List.member v rock then
                '@'
            else
            case Set.member v board of
                True -> '#'
                _ -> '.'
    in
    foldl (\v a -> (unitToChar v) :: a) [] points
        |> List.reverse
        |> LE.groupsOf w
        |> map String.fromList
        |> String.join "\n"

--------------------------------------------------------------

part2 input =
   input
        |> parseInput

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
