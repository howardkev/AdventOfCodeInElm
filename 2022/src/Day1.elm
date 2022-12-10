module Day1 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List exposing (map, filterMap, sum, maximum, sortWith, take)
import String exposing (split)

todayDescription : PuzzleDescription
todayDescription = { day = 1, title = "Calorie Counting" }

sampleInput : String
sampleInput =
    """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

part1 : String -> Maybe Int
part1 input =
    input
        |> parseInput
        |> maximum

parseInput : String -> List Int
parseInput input =
    input
        |> split "\n\n"
        |> map parseGroup

parseGroup : String -> Int
parseGroup group =
    group
        |> split "\n"
        |> filterMap String.toInt
        |> sum

-------------------------------------------------

part2 : String -> Int
part2 input =
    input
        |> parseInput
        |> sortWith descending
        |> take 3
        |> sum

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