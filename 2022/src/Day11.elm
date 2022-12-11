module Day11 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import Set exposing (Set)
import List exposing (map, filterMap, foldl, take, filter, concat, length, range, sort, sum, maximum, sortWith, head, tail, any, all, reverse)
import String exposing (lines, split, toList)
import Dict exposing (empty)
import Fifo
import BigInt exposing (..)

todayDescription : PuzzleDescription
todayDescription = { day = 11, title = "Monkey in the Middle" }

sampleInput : String
sampleInput =
    """
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"""

type alias Monkey =
    { id : Int
    , items : List BigInt
    , operation : Operation
    , divisibleBy : Int
    , trueThrow : Int
    , falseThrow : Int
    , inspected : Int
    , worryAdjust : BigInt }

type Operation
    = Add Int
    | Multiply Int
    | Square

part1 : String -> Int
part1 input =
    input
        |> parseInput (BigInt.fromInt 3)
        |> runNTimes 20 round
        |> Dict.values
        |> map .inspected
        |> sortWith descending
        |> take 2
        |> List.product

parseInput : BigInt -> String -> Dict Int Monkey
parseInput worryAdjust input =
    input
        |> split "\n\n"
        |> filterMap (toMonkey worryAdjust)
        |> toDictionary

toMonkey : BigInt -> String -> Maybe Monkey
toMonkey worryAdjust string =
    string
        |> String.lines
        |> parseDetails worryAdjust

parseDetails : BigInt -> List String -> Maybe Monkey
parseDetails worryAdjust lines =
    case lines of
        [monkey, items, operation, test, true, false] 
            -> Just 
                { id = head (ints monkey) |> Maybe.withDefault 0
                , items = map BigInt.fromInt (ints items) 
                , operation = case String.words operation of
                    ["Operation:", "new", "=", "old", "*", "old"] 
                        -> Square
                    ["Operation:", "new", "=", "old", "*", n] 
                        -> Multiply (String.toInt n |> Maybe.withDefault 0)
                    ["Operation:", "new", "=", "old", "+", n] 
                        -> Add (String.toInt n |> Maybe.withDefault 0)
                    _ -> Debug.todo "bad data"
                , divisibleBy = head (ints test) |> Maybe.withDefault 0
                , trueThrow = head (ints true) |> Maybe.withDefault 0
                , falseThrow = head (ints false) |> Maybe.withDefault 0
                , inspected = 0
                , worryAdjust = worryAdjust
                }
        _ -> Nothing

toDictionary : List Monkey -> Dict Int Monkey
toDictionary monkeys =
    let
        ids = map .id monkeys
    in
    LE.zip ids monkeys
        |> Dict.fromList

round : Dict Int Monkey -> Dict Int Monkey
round monkeys =
    foldl processOneMonkey monkeys (Dict.keys monkeys)

processOneMonkey : Int -> Dict Int Monkey -> Dict Int Monkey
processOneMonkey id monkeys =
    case Dict.get id monkeys of
        Just monkey ->
            let
                itemCount = List.length monkey.items
            in
            runNTimes itemCount (processItem id) monkeys
        _ -> monkeys

processItem : Int -> Dict Int Monkey -> Dict Int Monkey
processItem id monkeys =
    case Dict.get id monkeys of
        Just monkey 
            -> case monkey.items of
                    item :: remain ->
                        let
                            commonMod = map .divisibleBy (Dict.values monkeys)
                                |> List.product
                                |> BigInt.fromInt
                            dest = destMonkey monkey item commonMod
                            updated = updateMonkey dest monkeys
                            newMonkey = { monkey | items = remain, inspected = monkey.inspected + 1 }
                            maybeUpdate _ =
                                Just newMonkey
                            maybeUpdate2 _ =
                                Just updated
                            sourceDict = Dict.update id maybeUpdate monkeys
                        in
                        Dict.update updated.id maybeUpdate2 sourceDict
                    _ -> monkeys
        _ -> monkeys

updateMonkey : (Int, BigInt) -> Dict Int Monkey -> Monkey
updateMonkey (id, item) monkeys =
    case Dict.get id monkeys of
        Just monkey -> { monkey | items = monkey.items ++ [item] }
        _ -> Debug.todo "bad case"

destMonkey : Monkey -> BigInt -> BigInt -> (Int, BigInt)
destMonkey monkey item commonMultiple =
    let
        worry = case monkey.operation of
            Multiply n -> BigInt.mul item (BigInt.fromInt n)
            Add n -> BigInt.add item (BigInt.fromInt n)
            Square -> BigInt.mul item item
        bored = 
            if  monkey.worryAdjust == (BigInt.fromInt 1) then
                case BigInt.modBy commonMultiple worry of
                    Just n -> n
                    _ -> Debug.todo "mod error"
            else
                BigInt.div worry (BigInt.fromInt 3)
    in
    case BigInt.modBy (BigInt.fromInt monkey.divisibleBy) bored of
        Just n -> if n == (BigInt.fromInt 0) then
                (monkey.trueThrow, bored)
            else
                (monkey.falseThrow, bored)
        _ -> Debug.todo "error"

--------------------------------------------------------------

part2 : String -> String
part2 input =
    input
        |> parseInput (BigInt.fromInt 1)
        |> runNTimes 10000 round
        |> Dict.values
        |> map .inspected
        |> sortWith descending
        |> score

score : List Int -> String
score interactions =
    let
        top2 = interactions
            |> take 2
            |> map BigInt.fromInt
        bigProd = case top2 of
            [a, b] -> BigInt.mul a b
            _ -> BigInt.fromInt 0
    in
    bigProd |> BigInt.toString

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
