module Day25 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import List exposing (map, filterMap, foldl, take, filter, concat, length, range, sort, maximum, sortWith, head, tail, any, all, reverse)
import Parser exposing (Parser, Trailing(..))
import Debug as D
import Fifo exposing (Fifo)
import Set exposing (Set)

todayDescription : PuzzleDescription
todayDescription = { day = 25, title = "Full of Hot Air" }

sampleInput : String
sampleInput =
    String.trim """
1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
"""

part1 : String -> String
part1 input =
    input
        |> parseInput
        |> map toDecimal
        |> List.sum
        |> toSnafuDecimal
        |> toSnafu

parseInput : String -> List (List Char)
parseInput input =
    input
        |> String.split "\n"
        |> map String.toList

toSnafuDecimal : Int -> List Int
toSnafuDecimal number =
    foldl (\i (num, list) ->
                (modBy (5^i) number, (num // (5^i)) :: list) 
            ) (number, []) (reverse (range 0 20))
            |> Tuple.second
            |> reverse

toSnafu : List Int -> String
toSnafu digits =
    digits
        |> List.foldr (\digit (carry, str) ->
                case carry + digit of
                    0 -> (0, '0' :: str)
                    1 -> (0, '1' :: str)
                    2 -> (0, '2' :: str)
                    3 -> (1, '=' :: str)
                    4 -> (1, '-' :: str)
                    _ -> (carry, str)
            ) (0, [])
        |> Tuple.second
        |> LE.dropWhile (\c -> c == '0')
        |> String.fromList

toDecimal : List Char -> number
toDecimal line =
    line
        |> reverse
        |> foldl (\char (total, index) ->
                case char of
                    '1' -> (total + (5^index), index + 1)
                    '2' -> (total + (2 * (5^index)), index + 1)
                    '0' -> (total, index + 1)
                    '=' -> (total - (5^index * 2), index + 1)
                    '-' -> (total - (5^index), index + 1)
                    _ -> (total, index + 1)
            ) (0, 0)
        |> Tuple.first

--------------------------------------------------------------

part2 : a -> String
part2 _ =
    "Got 50 Stars!"

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
