module Day2 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import Set exposing (Set)
import List exposing (map, filterMap, foldl, filter, concat, length, range, sort, sum, maximum, sortWith)
import String exposing (split, toList)

todayDescription : PuzzleDescription
todayDescription = { day = 2, title = "Rock Paper Scissors" }

sampleInput : String
sampleInput =
    """
A Y
B X
C Z
"""

type HandShape = Rock | Paper | Scissors
type Result = Win | Lose | Draw
type alias Hands = (HandShape, HandShape)

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> filterMap toHandShapes
        |> map score
        |> sum

parseInput : String -> List (List String)
parseInput input =
    input
        |> split "\n"
        |> map (String.split " ")

toHandShapes : List String -> Maybe Hands
toHandShapes pair =
    case pair of
        [oponent, me] -> Maybe.map2 Tuple.pair (toHand oponent) (toHand me)
        _ -> Nothing

toHand : String -> Maybe HandShape
toHand letter =
    if letter == "A" || letter == "X" then
        Just Rock
    else if letter == "B" || letter == "Y" then
        Just Paper
    else if letter == "C" || letter == "Z" then
        Just Scissors
    else
        Nothing

score : Hands -> Int
score round =
    let
        resultPoints = getResultPoints <| getMyResult round
        handPoints = getHandPoints <| Tuple.second round
    in
        resultPoints + handPoints

getMyResult : Hands -> Result
getMyResult hands =
    case hands of
        (Rock, Paper) -> Win
        (Paper, Scissors) -> Win
        (Scissors, Rock) -> Win
        (Rock, Scissors) -> Lose
        (Paper, Rock) -> Lose
        (Scissors, Paper) -> Lose
        (Rock, Rock) -> Draw
        (Paper, Paper) -> Draw
        (Scissors, Scissors) -> Draw

getResultPoints : Result -> Int
getResultPoints result =
    case result of
        Win -> 6
        Lose -> 0
        Draw -> 3

getHandPoints : HandShape -> Int
getHandPoints hand =
    case hand of
        Rock -> 1
        Paper -> 2
        Scissors -> 3

-----------------------------------------------------

part2 : String -> Int
part2 input =
    input
        |> parseInput
        |> filterMap toHandsFromResult
        |> map score
        |> sum

toHandsFromResult : List String -> Maybe Hands
toHandsFromResult pair =
    let
        toResultHand : (Maybe HandShape) -> (Maybe Result) -> Maybe Hands
        toResultHand hand result =
            Maybe.map2 Tuple.pair
                hand
                (Maybe.map2 whatShouldPlay hand result)
    in
    case pair of
        [oponent, me] ->
            toResultHand (toHand oponent) (toResult me)
        _ -> Nothing

toResult : String -> Maybe Result
toResult item =
    case item of
        "X" -> Just Lose
        "Y" -> Just Draw
        "Z" -> Just Win
        _ -> Nothing

whatShouldPlay : HandShape -> Result -> HandShape
whatShouldPlay hand desiredResult =
    case (hand, desiredResult) of
        (Rock, Lose) -> Scissors
        (Rock, Draw) -> Rock
        (Rock, Win) -> Paper
        (Paper, Lose) -> Rock
        (Paper, Draw) -> Paper
        (Paper, Win) -> Scissors
        (Scissors, Lose) -> Paper
        (Scissors, Draw) -> Scissors
        (Scissors, Win) -> Rock

-------------------------------------------------

init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = String.trim sampleInput
        , puzzleInput = Nothing
        , result = Nothing
        , description = todayDescription }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        p1 = Just (Debug.toString (part1 model.input))
        p2 = Just (Debug.toString (part2 model.input))
    in
        mainUpdate msg model sampleInput p1 p2
    
main : Program () Model Msg
main =
  Browser.element 
    { init = init
    , update = update 
    , view = view 
    , subscriptions = subscriptions }
