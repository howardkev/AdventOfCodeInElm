module Day21 exposing (..)

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
import Html.Attributes exposing (list)
import Html exposing (a)
import Parser exposing (int)
import BigInt exposing (BigInt)

todayDescription : PuzzleDescription
todayDescription = { day = 21, title = "Monkey Math" }

sampleInput : String
sampleInput =
    String.trim """
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
"""

type MonkeyType
    = Yell Float
    | Add String String
    | Multiply String String
    | Subtract String String
    | Divide String String
    | Human

type alias Monkey = (String, MonkeyType)
type alias State = Dict String MonkeyType

part1 : String -> Maybe Float
part1 input =
    input
        |> parseInput
        |> calculate "root"

parseInput : String -> State
parseInput input =
    input
        |> String.split "\n"
        |> filterMap toMonkey
        |> Dict.fromList

toMonkey : String -> Maybe Monkey
toMonkey line =
    let
        extractName string = String.dropRight 1 string
    in
    case String.words line of
        [a, b] -> Just ( extractName a, Yell  (String.toFloat b |> Maybe.withDefault 0))
        [a, b, "*", d] -> Just (extractName a, Multiply b d)
        [a, b, "/", d] -> Just (extractName a, Divide b d)
        [a, b, "-", d] -> Just (extractName a, Subtract b d)
        [a, b, "+", d] -> Just (extractName a, Add b d)
        _ -> Nothing

calculate : String -> State -> Maybe Float
calculate monkeyName dict =
    let
        getCommand name =
            dict
                |> Dict.get name

        lookup name =
            case Dict.get name dict of
                Just (Yell n) -> Just n
                _ -> calculate name dict

        valueOf command = case command of
            Multiply a b ->
                Maybe.map2 (*) (lookup a) (lookup b)

            Add a b ->
                Maybe.map2 (+) (lookup a) (lookup b)
                
            Subtract a b ->
                Maybe.map2 (-) (lookup a) (lookup b)
                
            Divide a b -> 
                Maybe.map2 (/) (lookup a) (lookup b)

            Yell n -> Just n

            Human -> Nothing
    in
    getCommand monkeyName
        |> Maybe.andThen valueOf

--------------------------------------------------------------

part2 : String -> Maybe Float
part2 input =
    input
        |> parseInput
        |> solve

solve : State -> Maybe Float
solve dict =
    let
        root = dict
            |> Dict.get "root"
            |> Maybe.andThen (\monkey ->
                    case monkey of
                        Add a b -> Just (a, b)
                        _ -> Nothing
                )

        humanRemoved = Dict.filter (\k _ -> k /= "humn") dict
    in
    case root of
        Just (name1, name2) ->
            let
                firstResult = calculate name1 humanRemoved
                secondResult = calculate name2 humanRemoved
            in
            case (firstResult, secondResult) of
                (Nothing, Just target) ->
                    reverseCalculate humanRemoved name1 target
                (Just target, Nothing) ->
                    reverseCalculate humanRemoved name2 target
                _ -> Nothing

        _ -> Nothing

reverseCalculate : State -> String -> Float -> Maybe Float
reverseCalculate dict monkeyName target =
    let
        getCommand name =
            dict
                |> Dict.get name
                |> Maybe.withDefault Human

        lookup name =
            case Dict.get name dict of
                Just (Yell n) -> Just n
                _ -> calculate name dict

        reverseOf command = case command of
            Divide n1 n2 ->
                case (lookup n1, lookup n2) of
                    (Nothing, Just v2) ->
                        reverseCalculate dict n1 (target * v2)
                    (Just v1, Nothing) ->
                        reverseCalculate dict n2 (v1 / target)
                    _ -> Nothing

            Add n1 n2 ->
                case (lookup n1, lookup n2) of
                    (Nothing, Just v2) ->
                        reverseCalculate dict n1 (target - v2)
                    (Just v1, Nothing) ->
                        reverseCalculate dict n2 (target - v1)
                    _ -> Nothing

            Multiply n1 n2 ->
                case (lookup n1, lookup n2) of
                    (Nothing, Just v2) ->
                        reverseCalculate dict n1 (target / v2)
                    (Just v1, Nothing) ->
                        reverseCalculate dict n2 (target / v1)
                    _ -> Nothing

            Subtract n1 n2 ->
                case (lookup n1, lookup n2) of
                    (Nothing, Just v2) ->
                        reverseCalculate dict n1 (v2 + target)
                    (Just v1, Nothing) ->
                        reverseCalculate dict n2 (v1 - target)
                    _ -> Nothing

            Human -> Just target
            
            _ -> Nothing
    in
    getCommand monkeyName
        |> reverseOf

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
