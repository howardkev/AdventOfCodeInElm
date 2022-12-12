module Day5 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import Set exposing (Set)
import List exposing (map, filterMap, foldl, filter, concat, length, range, sort, sum, maximum, sortWith, head, tail)
import String exposing (lines, split, toList)
import Element exposing (row)
import Dict exposing (empty)
import Element exposing (Attribute)
import Fifo

todayDescription : PuzzleDescription
todayDescription = { day = 5, title = "Supply Stacks" }

sampleInput : String
sampleInput =
    """
move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""

initial2 : List (List Char)
initial2 = [['N', 'Z'], 
          ['D', 'C', 'M'], 
          ['P']]

initial : List (List Char)
initial = [['G', 'W', 'L', 'J', 'B', 'R', 'T', 'D'], 
          ['C', 'W', 'S'], 
          ['M', 'T', 'Z', 'R'],
          ['V', 'P', 'S', 'H', 'C', 'T', 'D'],
          ['Z', 'D', 'L', 'T', 'P', 'G'],
          ['D', 'C', 'Q', 'J', 'Z', 'R', 'B', 'F'],
          ['R', 'T', 'F', 'M', 'J', 'D', 'B', 'S'],
          ['M', 'V', 'T', 'B', 'R', 'H', 'L'],
          ['V', 'S', 'D', 'P', 'Q']
          ]

parseInput : String -> List (List Int)
parseInput input =
    input
        |> split "\n"
        |> map ints

one : List Int -> List (List Char) -> List (List Char)
one move acc =
    case move of
        [_, b, c] ->
            let
                sourceList = Maybe.withDefault [] (LE.getAt (b - 1) acc)
                moving = Maybe.withDefault 'X' (head sourceList)
                newSource = Maybe.withDefault [] (tail sourceList)
                destList = Maybe.withDefault [] (LE.getAt (c - 1) acc)
                newDest = moving :: destList
                result = LE.setAt (b - 1) newSource acc
                    |> LE.setAt (c - 1) newDest
            in
                result
        _ -> Debug.todo "here"

oneMove : List Int -> List (List Char) -> List (List Char)
oneMove move acc =
    let
        count = case move of
            [a, _, _] -> a
            _ -> 0
    in
    List.foldl (\_ b -> one move b) acc (range 0 (count - 1))

moveCrates : List (List Int) -> List (List Char)
moveCrates moves =
    List.foldl oneMove initial moves

score : List (List Char) -> List Char
score boxes = 
    map (\box -> Maybe.withDefault 'X' (head box)) boxes

part1 : String -> String
part1 input =
    input
        |> parseInput
        |> moveCrates
        |> score
        |> String.fromList

one2 : List Int -> List (List Char) -> List (List Char)
one2 move acc =
    case move of
        [a, b, c] ->
            let
                sourceList = Maybe.withDefault [] (LE.getAt (b - 1) acc)
                moving = List.take a sourceList
                newSource = List.drop a sourceList
                destList = Maybe.withDefault [] (LE.getAt (c - 1) acc)
                newDest = moving ++ destList
                result = LE.setAt (b - 1) newSource acc
                    |> LE.setAt (c - 1) newDest
            in
                result
        _ -> Debug.todo "here"

oneMove2 : List Int -> List (List Char) -> List (List Char)
oneMove2 move acc =
    let
        count = case move of
            [a, _, _] -> a
            _ -> 0
    in
    List.foldl (\_ b -> one2 move b) acc (range 0 (count - 1))

moveCrates2 : List (List Int) -> List (List Char)
moveCrates2 moves =
    List.foldl one2 initial moves

part2 : String -> String
part2 input =
    input
        |> parseInput
        |> moveCrates2
        |> score
        |> String.fromList

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
