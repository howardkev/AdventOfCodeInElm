module Day4 exposing (..)

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
todayDescription = { day = 4, title = "Camp Cleanup" }

sampleInput : String
sampleInput =
    """
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
"""

parseInput input =
    input
        |> split "\n"
        |> map ints
        --|> map (String.split " ")
        --|> map String.toList

toRanges pair =
    case pair of
        [a,b,c,d] -> (Set.fromList (List.range a b), Set.fromList (List.range c d))
        _ -> Debug.todo "bad input"

check (a, b) =
    let
        inter = Set.intersect a b
    in
        inter == a || inter == b

part1 input =
    input
        |> parseInput
        |> map toRanges
        |> map check
        |> countTrue

check2 (a, b) =
    let
        inter = Set.intersect a b
    in
        Set.size inter > 0

part2 input =
    input
        |> parseInput
        |> map toRanges
        |> map check2
        |> countTrue

-------------------------------------------------
isPalindrome : String -> Bool
isPalindrome str =
    case String.uncons str of
        Nothing -> True
        Just (_, "") -> True
        Just (hd, tail) -> 
            let
                last = String.right 1 tail
                middle = String.dropRight 1 tail
            in
                String.fromChar hd == last && isPalindrome middle

sum1 : List number -> number
sum1 values =
    case values of
        [] -> 0
        x :: xs -> x + sum xs

rev : String -> String
rev str =
    case String.uncons str of
        Nothing -> ""
        Just (hd, tail) -> rev tail ++ String.fromChar hd

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
