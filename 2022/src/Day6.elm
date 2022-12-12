module Day6 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Set exposing (Set)
import List exposing (map)
import String exposing (toList)

todayDescription : PuzzleDescription
todayDescription = { day = 6, title = "Tuning Trouble" }

sampleInput : String
sampleInput =
    """
mjqjpqmgbljsphdztnvjfqwrcgsmlb
"""

parseInput : String -> List Char
parseInput input =
    input
        |> toList

solve : Int -> List Char -> Maybe Int
solve size input = 
    input
        |> LE.groupsOfWithStep size 1
        |> map Set.fromList
        |> LE.findIndex (\a -> Set.size a == size)
        |> Maybe.map (\a -> size + a)

part1 : String -> Maybe Int
part1 input =
    input
        |> parseInput
        |> solve 4

part2 : String -> Maybe Int
part2 input =
    input
        |> parseInput
        |> solve 14

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
