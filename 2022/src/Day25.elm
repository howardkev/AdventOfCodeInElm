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
        |> foldl sumSnafu ['0']
        |> String.fromList

parseInput : String -> List (List Char)
parseInput input =
    input
        |> String.split "\n"
        |> map String.toList

sumSnafu : List Char -> List Char -> List Char
sumSnafu n1 n2 =
    let
        maxLen = max (length n1) (length n2) + 1
        normalize n = 
            List.repeat (maxLen - (length n)) '0' ++ n
                |> reverse
        convert c = case c of
            '0' -> 0
            '1' -> 1
            '2' -> 2
            '-' -> -1
            '=' -> -2
            _ -> 0
    in
    LE.zip (normalize n1) (normalize n2)
        |> foldl (\(v1,v2) (carry, result) ->
                case (convert v1) + (convert v2) + carry of
                    0 -> (0, '0' :: result)
                    1 -> (0, '1' :: result)
                    2 -> (0, '2' :: result)
                    3 -> (1, '=' :: result)
                    4 -> (1, '-' :: result)
                    5 -> (1, '0' :: result)
                    i -> 
                        if i == -1 then
                            (0, '-' :: result)
                        else if i == -2 then
                            (0, '=' :: result)
                        else if i == -3 then
                            (-1, '2' :: result)
                        else if i == -4 then
                            (-1, '1' :: result)
                        else --if i == -5 then
                            (-1, '0' :: result)
            ) (0, [])
        |> Tuple.second
        |> LE.dropWhile (\c -> c == '0')

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
