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
todayDescription = { day = 25, title = "---" }

sampleInput : String
sampleInput =
    String.trim """
1
2
3
"""

part1 input =
    input
        |> parseInput

parseInput input =
    input
        |> String.split "\n"

--------------------------------------------------------------

part2 input =
    input
        |> parseInput

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
