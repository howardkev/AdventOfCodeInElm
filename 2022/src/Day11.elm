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
import Array exposing (Array)

todayDescription : PuzzleDescription
todayDescription = { day = 11, title = "Monkey in the Middle" }

sampleInput : String
sampleInput =
    String.trim """
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
    , items : List Item
    , operation : Operation
    , divisibleBy : Int
    , trueThrow : Int
    , falseThrow : Int
    , inspected : Int }

type Operation
    = Add Int
    | Multiply Int
    | Square
    
type alias State =
    { monkeys : Array Monkey
    , worryAdjust : BigInt
    , commonMod : BigInt }

type alias Item = BigInt

part1 : String -> String
part1 input =
    input
        |> parseInput
        |> initialState (BigInt.fromInt 3)
        |> runNTimes 20 round
        |> .monkeys
        |> Array.map .inspected
        |> Array.toList
        |> score

parseInput : String -> Array Monkey
parseInput input =
    input
        |> String.split "\n\n"
        |> List.filterMap toMonkey
        |> Array.fromList

toMonkey : String -> Maybe Monkey
toMonkey string =
    string
        |> String.lines
        |> parseDetails
        
parseDetails : List String -> Maybe Monkey
parseDetails lines =
    case lines of
        [monkey, items, operation, test, true, false] 
            -> Just 
                { id = List.head (ints monkey) |> Maybe.withDefault 0
                , items = List.map BigInt.fromInt (ints items) 
                , operation = case String.words operation of
                    ["Operation:", "new", "=", "old", "*", "old"] 
                        -> Square
                    ["Operation:", "new", "=", "old", "*", n] 
                        -> Multiply (String.toInt n |> Maybe.withDefault 0)
                    ["Operation:", "new", "=", "old", "+", n] 
                        -> Add (String.toInt n |> Maybe.withDefault 0)
                    _ -> Debug.todo "bad data"
                , divisibleBy = List.head (ints test) |> Maybe.withDefault 0
                , trueThrow = List.head (ints true) |> Maybe.withDefault 0
                , falseThrow = List.head (ints false) |> Maybe.withDefault 0
                , inspected = 0
                }
        _ -> Nothing

initialState : BigInt -> Array Monkey -> State
initialState worryAdjust monkeys =
    { monkeys = monkeys
    , worryAdjust = worryAdjust
    , commonMod = Array.map .divisibleBy monkeys
          |> arrayProduct
          |> BigInt.fromInt }
          
arrayProduct : Array Int -> Int
arrayProduct arr =
    arr |> Array.toList |> List.product
    
round : State -> State
round state =
    let
        ids = List.range 0 (Array.length state.monkeys)
    in
    List.foldl processOneMonkey state ids
    
processOneMonkey : Int -> State -> State
processOneMonkey id state =
    case Array.get id state.monkeys of
        Just monkey ->
            processItems monkey state
        _ -> state
        
processItems : Monkey -> State -> State
processItems monkey state =
    case monkey.items of
        [] -> state
        item :: nextItems ->
            let
                currentMonkey = { monkey | 
                    items = nextItems, inspected = monkey.inspected + 1 }
                destTarget = 
                    getDestMonkey monkey item state
                destMonkey = 
                    addItemToMonkey destTarget state
                nextState = { state | 
                    monkeys = Array.set monkey.id currentMonkey state.monkeys }
                nextState2 = { nextState | 
                    monkeys = Array.set destMonkey.id destMonkey nextState.monkeys }
            in
            processItems currentMonkey nextState2
            
addItemToMonkey : (Int, Item) -> State -> Monkey
addItemToMonkey (id, item) state =
    case Array.get id state.monkeys of
        Just monkey ->
            { monkey | items = monkey.items ++ [item] }
        _ -> Debug.todo "bad target monkey"
            
getDestMonkey : Monkey -> BigInt -> State -> (Int, Item)
getDestMonkey monkey item state =
    let
        worry = 
            case monkey.operation of
                Multiply n -> BigInt.mul item (BigInt.fromInt n)
                Add n -> BigInt.add item (BigInt.fromInt n)
                Square -> BigInt.mul item item
        bored =
            if  state.worryAdjust == (BigInt.fromInt 1) then
                case BigInt.modBy state.commonMod worry of
                    Just n -> n
                    _ -> Debug.todo "mod error"
            else
                BigInt.div worry state.worryAdjust
    in
    case BigInt.modBy (BigInt.fromInt monkey.divisibleBy) bored of
        Just n -> 
            if n == (BigInt.fromInt 0) then
                (monkey.trueThrow, bored)
            else
                (monkey.falseThrow, bored)
        _ -> Debug.todo "mod error"
        
score : List Int -> String
score interactions =
    let
        top2 = interactions
            |> List.sortWith descending
            |> List.take 2
            |> List.map BigInt.fromInt
        bigProd = case top2 of
            [a, b] -> BigInt.mul a b
            _ -> BigInt.fromInt 0
    in
    bigProd |> BigInt.toString

--------------------------------------------------------------

part2 : String -> String
part2 input =
    input
        |> parseInput
        |> initialState (BigInt.fromInt 1)
        |> runNTimes 10000 round
        |> .monkeys
        |> Array.map .inspected
        |> Array.toList
        |> score

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
