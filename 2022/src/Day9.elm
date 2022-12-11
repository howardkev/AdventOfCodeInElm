module Day9 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import Set exposing (Set)
import List exposing (map, filterMap, foldl, filter, concat, length, range, sort, sum, maximum, sortWith, head, tail, any, all, reverse)
import String exposing (lines, split, toList)
import Dict exposing (empty)
import Fifo

todayDescription : PuzzleDescription
todayDescription = { day = 9, title = "Rope Bridge" }

sampleInput : String
sampleInput =
    """
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"""

type alias Command = 
    { steps: Int
    , direction : Direction }

type Direction = Up | Down | Left | Right

type alias State =
    { h : Coordinate
    , t : Coordinate
    , visited : List Coordinate }

type alias Coordinate = (Int, Int)

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> foldl executeCommand initialState
        |> .visited
        |> Set.fromList
        |> Set.size

parseInput : String -> List Command
parseInput input =
    input
        |> split "\n"
        |> filterMap toCommand

toCommand : String -> Maybe Command
toCommand line =
    case String.words line of
        [dir, n] -> 
            Maybe.map2 Command (String.toInt n) (toDirection dir)
        _ -> Nothing

toDirection : String -> Maybe Direction
toDirection char =
    case char of
        "R" -> Just Right
        "U" -> Just Up
        "L" -> Just Left
        "D" -> Just Down
        _ -> Nothing 

initialState : State
initialState =
    { h = (0, 0)
    , t = (0, 0)
    , visited = List.singleton (0,0) }

executeCommand : Command -> State -> State
executeCommand command state =
    runNTimes command.steps (singleStep command.direction) state

singleStep : Direction -> State -> State
singleStep direction ({ h, t, visited } as state) =
    let
        newH = moveHead direction h
        newT = moveTail newH t
    in
    { state | h = newH, t = newT, visited = newT :: visited }

moveHead : Direction -> Coordinate -> Coordinate
moveHead direction (hx, hy) =
    case direction of
        Right -> (hx + 1, hy)
        Left -> (hx - 1, hy)
        Up -> (hx, hy - 1)
        Down -> (hx, hy + 1)

moveTail : Coordinate -> Coordinate -> Coordinate
moveTail ((hx, hy) as h) ((tx, ty) as t) =
    let
        withinOne : Bool
        withinOne = abs (hx - tx) <= 1 && abs (hy - ty) <= 1

        sameRowOrColumn : Bool
        sameRowOrColumn = hx == tx || hy == ty
    in
    if withinOne then
        t
    else if sameRowOrColumn then
        moveTailHorzVert h t
    else
        moveTailDiag h t

moveTailHorzVert : Coordinate -> Coordinate -> Coordinate
moveTailHorzVert (hx, hy) (tx, ty) =
    if tx < hx then
        (tx + 1, ty)
    else if tx > hx then
        (tx - 1, ty)
    else if ty < hy then
        (tx, ty + 1)
    else --if ty > hy then
        (tx, ty - 1)

moveTailDiag : Coordinate -> Coordinate -> Coordinate
moveTailDiag (hx, hy) (tx, ty) =
    if tx < hx && ty > hy then
        (tx + 1, ty - 1)
    else if tx < hx && ty < hy then
        (tx + 1, ty + 1)
    else if tx > hx && ty > hy then
        (tx - 1, ty - 1)
    else --if tx > hx && ty < hy then
        (tx - 1, ty + 1)

--------------------------------------------------------------

part2 : String -> Int
part2 input =
    input
        |> parseInput
        |> foldl executeCommand initialState
        |> .visited
        |> reverse
        |> runNTimes 8 solveNextInChain
        |> Set.fromList
        |> Set.size

solveNextInChain : List Coordinate -> List Coordinate
solveNextInChain visited =
    LE.scanl moveTail (0,0) visited

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
