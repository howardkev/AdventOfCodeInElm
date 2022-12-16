module Util exposing (..)

import List.Extra as LE
import Regex
import Dict exposing (Dict)
import Set exposing (Set)
import List exposing (map, filterMap, foldl, filter, concat, length, range, sort, sum, maximum, sortWith, head, tail)
import String exposing (lines, split, toList)

ints : String -> List Int
ints input =
  let 
    regex = Maybe.withDefault Regex.never <|
      Regex.fromString "-?[0-9]+"
    found = Regex.find regex
  in
    List.filterMap (.match >> String.toInt) (found input)

collectFirsts : List (a, b) -> List a
collectFirsts =
    List.map Tuple.first

collectSeconds : List (a, b) -> List b
collectSeconds =
    List.map Tuple.second

countTrue : List Bool -> Int
countTrue =
    LE.count ((==) True)

descending : comparable -> comparable -> Order
descending a b =
    case compare a b of
        LT -> GT
        EQ -> EQ
        GT -> LT

pointGrid : Int -> Int -> Int -> Int -> List (Int, Int)
pointGrid x y width height =
    let
        xs = List.range x (x + width - 1)
        ys = List.range y (y + height - 1)
    in
        ys |> List.concatMap (\y_ -> 
            xs |> List.concatMap (\x_ -> [(x_, y_)]))

type alias Grid a =
    { width : Int
    , height : Int
    , units : Dict (Int, Int) a
    }

parseGrid : List String -> Grid Char
parseGrid lines =
    let
        numRows = length lines
        numCols = head lines 
            |> Maybe.withDefault "" 
            |> String.length
        caveValues = lines
            |> map toList
            |> List.indexedMap Tuple.pair
    in
        { width = numCols
        , height = numRows
        , units = caveValues |> map expand |> foldl Dict.union Dict.empty }

expand : (Int, List a) -> Dict (Int, Int) a
expand row =
    let
        number = Tuple.first row
        chars = Tuple.second row
        cols = range 0 (length chars)
        helper (a, b) grid =
            Dict.insert (number, a) b grid
    in
        LE.zip cols chars
        |> foldl helper Dict.empty

runNTimes : Int -> (c -> c) -> c -> c
runNTimes n f state =
    List.foldl (\_ acc -> f acc) state (range 1 n)
    