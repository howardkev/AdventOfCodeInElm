module Day10a exposing (..)

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
    """
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

type alias State = 
    { x : Int
    , cycle : Int
    , score : Int
    , crt : String }

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> foldl executeInstruction initialState
        |> .score

parseInput : String -> List Instruction
parseInput input =
    input
        |> split "\n"
        |> filterMap toInstruction
        |> concat

toInstruction : String -> Maybe (List Instruction)
toInstruction line =
    case String.words line of
        ["noop"] -> Just [Nop]
        ["addx", n] -> Just [Nop, (Addx (String.toInt n |> Maybe.withDefault 0))]
        _ -> Nothing

initialState : State
initialState = 
    { x = 1, cycle = 1, score = 0, crt = "" }

executeInstruction : Instruction -> State -> State
executeInstruction instruction state =
    let
        scoreAddition =
            if List.member state.cycle [ 20, 60, 100, 140, 180, 220 ] then
                state.cycle * state.x
            else
                0
        add = case instruction of
            Addx n -> n
            _ -> 0
    in
    { state 
    | score = state.score + scoreAddition
    , cycle = state.cycle + 1
    , x = state.x + add }

--------------------------------------------------------------

part2 : String -> String
part2 input =
    input
        |> parseInput
        |> foldl executeInstruction2 initialState
        |> .crt
        |> String.toList
        |> LE.groupsOf 40
        |> map String.fromList
        |> String.join "\n"

executeInstruction2 : Instruction -> State -> State
executeInstruction2 instruction state =
    let
        add = case instruction of
            Addx n -> n
            _ -> 0

        pixel =
            let
                pos = modBy 40 (state.cycle - 1)
            in
            if abs (pos - state.x) <= 1 then
                "#"
            else
                " "
    in
    { state 
    | cycle = state.cycle + 1
    , x = state.x + add
    , crt = state.crt ++ pixel } 

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
    let
        p1 = Just (toString (Debug.toString (part1 model.input)))
        p2 = Just (toString (Debug.toString (part2 model.input)))
    in
        mainUpdate msg model sampleInput p1 p2
    
main : Program () Model Msg
main =
  Browser.element 
    { init = init
    , update = update 
    , view = view 
    , subscriptions = subscriptions }
