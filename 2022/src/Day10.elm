module Day10 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import Set exposing (Set)
import List exposing (map, filterMap, foldl, take, filter, concat, length, range, sort, sum, maximum, sortWith, head, tail, any, all, reverse)
import String exposing (lines, split, toList)
import Dict exposing (empty)
import Fifo

todayDescription : PuzzleDescription
todayDescription = { day = 10, title = "Cathode-Ray Tube" }

sampleInput : String
sampleInput =
    String.trim """
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
"""

type Instruction
    = Nop
    | Addx Int

type alias RegisterX = Int
type alias Pixel = Char

type alias State = 
    { x : RegisterX
    , xs : List RegisterX }

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> foldl executeInstruction initialState
        |> .xs >> reverse
        |> score

parseInput : String -> List Instruction
parseInput input =
    input
        |> split "\n"
        |> filterMap toInstruction

toInstruction : String -> Maybe Instruction
toInstruction line =
    case String.words line of
        ["noop"] -> Just Nop
        ["addx", n] -> Just (Addx (String.toInt n |> Maybe.withDefault 0))
        _ -> Nothing

initialState : State
initialState = 
    { x = 1, xs = [0] }

executeInstruction : Instruction -> State -> State
executeInstruction instruction state =
    case instruction of
        Nop -> 
            { state | xs = state.x :: state.xs }
        Addx x ->
            let
                nextX = state.x + x
            in
            { state 
            | xs = [nextX, state.x] ++ state.xs 
            , x = nextX }

score : List RegisterX -> Int
score xs =
    let
        positions = [20, 60, 100, 140, 180, 220]
        getAtIndex pos = LE.getAt (pos - 1) xs
        cycles = filterMap getAtIndex positions
    in 
    List.map2 (*) positions cycles
        |> sum

--------------------------------------------------------------

part2 : String -> String
part2 input =
    input
        |> parseInput
        |> foldl executeInstruction initialState
        |> .xs >> reverse
        |> getCrt
        |> LE.groupsOf 40
        |> map String.fromList
        |> String.join "\n"

getCrt : List RegisterX -> List Pixel
getCrt xs =
    let
        getPixel cycle x =
            if abs (modBy 40 cycle - x) <= 1 then
                '#'
            else
                ' '
    in
    List.indexedMap getPixel xs

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
