module Day20 exposing (..)

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
import Html.Attributes exposing (list)
import Html exposing (a)
import Day10 exposing (score)
import Parser exposing (int)

todayDescription : PuzzleDescription
todayDescription = { day = 20, title = "Grove Positioning System" }

sampleInput : String
sampleInput =
    String.trim """
1
2
-3
3
-2
0
4
"""

type alias State = 
    { indexes : List Int
    , list : List Int
    }

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> toInitialState
        |> solve
        |> score

score : State -> Int
score state =
    let
        zeroIndex = LE.elemIndex 0 state.list 
            |> Maybe.withDefault 0

        getAtPosition  pos =
            let
                nextIndex = modBy (length state.list) (zeroIndex + pos)
            in
            LE.getAt nextIndex state.list
                |> Maybe.withDefault 0

        thousand = getAtPosition 1000
        twoThousand = getAtPosition 2000
        threeThousand = getAtPosition 3000
    in
    thousand + twoThousand + threeThousand

parseInput : String -> List Int
parseInput input =
    input
        |> String.split "\n"
        |> filterMap String.toInt

toInitialState : List Int -> State
toInitialState array =
    { list = array
    , indexes = List.range 0 (length array - 1)
    }

solve : State -> State
solve state =
    foldl oneStep state state.indexes

oneStep : Int -> State -> State
oneStep command state =
    let
        moveNumber index count list =
            if count == 0 then
                list
            else
                let
                    source = LE.getAt index list 
                        |> Maybe.withDefault 0
                    removed = LE.removeAt index list
                    dest = modBy (length list - 1) (index + count)
                    (l, r) = LE.splitAt dest removed
                    added = l ++ (source :: r) 
                in
                added

        indexPos = LE.elemIndex command state.indexes
            |> Maybe.withDefault 0

        numberToMove = LE.getAt indexPos state.list
            |> Maybe.withDefault 0

        newList = moveNumber indexPos numberToMove state.list
        newIndexes = moveNumber indexPos numberToMove state.indexes
    in
    { state 
        | indexes = newIndexes
        , list = newList 
    }

--------------------------------------------------------------

part2 : String -> Int
part2 input =
   input
        |> parseInput
        |> map ((*) 811589153)
        |> toInitialState
        |> solve2
        |> score

solve2 : State -> State
solve2 state =
    let
        commands = state.indexes
            |> List.repeat 10
            |> concat
    in
    foldl oneStep state commands

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
