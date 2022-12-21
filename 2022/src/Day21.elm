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
todayDescription = { day = 21, title = "---" }

sampleInput : String
sampleInput =
    """
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

type alias Monkey = (String, MonkeyType)

type alias State = Dict String MonkeyType

part1 : String -> Float
part1 input =
    input
        |> parseInput
        |> calculate "root" (-1) 

getValue : Maybe MonkeyType -> Float
getValue monkey =
    case monkey of
        Just (Yell n) -> n
        _ -> 0.0

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

calculate : String -> Float -> State -> Float
calculate id h dict =
    let
        getCommand name =
            dict
                |> Dict.get name
                |> Maybe.withDefault (Yell 0)

        lookup name =
            if name == "humn" && h >= 0 then
                h
            else
                case Dict.get name dict of
                    Just (Yell n) -> n
                    _ -> calculate name h dict

        helper v = case v of
            Multiply a b ->
                (lookup a) * (lookup b)

            Add a b ->
                (lookup a) + (lookup b)
                
            Subtract a b ->
                (lookup a) - (lookup b)
                
            Divide a b -> 
                (lookup a) / (lookup b)

            Yell n -> n
    in
    helper (getCommand id)

--------------------------------------------------------------

part2 : String -> Float
part2 input =
    input
        |> parseInput
        |> execute

execute : State -> Float
execute dict =
    let
        root = dict
            |> Dict.filter (\k _ -> k == "root")
            |> Dict.values

        (name1, name2) = case root of
            [Add a b] -> (a, b)
            _ -> ("","")

        firstZero = calculate name1 0 dict
        firstOne = calculate name1 100 dict
        secondZero = calculate name2 0 dict
        hi = 1e20
    in
    if firstZero == firstOne then
        binarySearch dict name2 firstZero 0 hi
    else
        binarySearch dict name1 secondZero 0 hi

binarySearch : State -> String -> Float -> Float -> Float -> Float
binarySearch dict name target lo hi =
    let
        middle = toFloat (round ((hi + lo) / 2))
        result = target - (calculate name middle dict)
    in
    if lo >= hi then
        middle
    else
        if result < 0 then
            binarySearch dict name target (middle + 1) hi
        else if result > 0 then
            binarySearch dict name target lo (middle - 1)
        else
            middle

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
