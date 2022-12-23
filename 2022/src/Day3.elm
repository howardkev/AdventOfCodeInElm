module Day3 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import Set exposing (Set)
import List exposing (map, concatMap, filterMap, foldl, filter, concat, length, range, sort, sum, maximum, sortWith)
import String exposing (split, toList)
import String exposing (lines)

todayDescription : PuzzleDescription
todayDescription = { day = 3, title = "Rucksack Reorganization" }

sampleInput : String
sampleInput =
    String.trim """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""

type alias Item = Char
type alias Rucksack = List Item

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> concatMap findItemsInCommon
        |> calcAnswer

parseInput : String -> List Rucksack
parseInput input =
    input
        |> split "\n"
        |> map String.toList

findItemsInCommon : Rucksack -> List Item
findItemsInCommon rucksack =
    rucksack
        |> splitRucksack
        |> inCommon
        |> Set.toList

splitRucksack : Rucksack -> (Set Item, Set Item)
splitRucksack rucksack =
    let 
        len = List.length rucksack // 2
        (firstHalf, secondHalf) = LE.splitAt len rucksack
    in
        (Set.fromList firstHalf, Set.fromList secondHalf)

inCommon : (Set Item, Set Item) -> Set Item
inCommon (a, b) =
    Set.intersect a b

calcAnswer : List Item -> Int
calcAnswer items =
    items
        |> map priority
        |> sum

priority : Item -> Int
priority item =
    if Char.isLower item then
        Char.toCode item - Char.toCode 'a' + 1
    else
        Char.toCode item - Char.toCode 'A' + 27

--------------------------------------------------------

part2 : String -> Int
part2 input =
    input
        |> parseInput
        |> map Set.fromList
        |> LE.groupsOf 3
        |> filterMap combineSets
        |> concatMap Set.toList
        |> calcAnswer

combineSets : List (Set Item) -> Maybe (Set Item)
combineSets xs =
    case xs of
        [a, b, c] -> 
            Set.intersect a b 
                |> Set.intersect c 
                |> Just
        _ -> Nothing

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
