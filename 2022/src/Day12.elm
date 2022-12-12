module Day12 exposing (..)

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
import GridExample exposing (Board)

todayDescription : PuzzleDescription
todayDescription = { day = 12, title = "Hill Climbing Algorithm" }

sampleInput : String
sampleInput =
    """
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"""

part1 input =
    input
        |> parseInput
        |> findPath
        |> score

parseInput input =
    input
        |> split "\n"
        |> parseBlock

parseBlock lines =
    let
        values = lines
            |> map toList
            |> List.indexedMap Tuple.pair
            |> map expand
            |> foldl Dict.union Dict.empty
        start = Dict.filter (\_ v -> v == 'S') values
            |> Dict.keys
            |> head
            |> Maybe.withDefault (0,0)
        end = Dict.filter (\_ v -> v == 'E') values
            |> Dict.keys
            |> head
            |> Maybe.withDefault (0,0)
    in
        { start = start
        , end = end
        , heights = values
        , next = Fifo.fromList [end]
        , visited = Set.singleton end
        , distances = Dict.fromList [(end, 0)]
        }

findPath board =
    case Fifo.remove board.next of
        (Nothing, _) -> board
        (Just top, remain) ->
            let
                (row, col) = top
                myHeightCode = 
                    case Dict.get top board.heights of
                        Just 'S' -> Char.toCode 'a'
                        Just 'E' -> Char.toCode 'z'
                        Just c -> Char.toCode c
                        Nothing -> Char.toCode 'a'
                adj = [(row - 1, col), (row + 1, col),
                        (row, col - 1), (row, col + 1)]
                validJump _ v =
                    let
                        checkChar = 
                            if v == 'E' then
                                Char.toCode 'z'
                            else if v == 'S' then
                                Char.toCode 'a'
                            else
                                Char.toCode v
                    in
                    checkChar >= myHeightCode - 1
                notVisited k _ =
                    not (Set.member k board.visited)
                values = getValues board.heights adj
                        |> Dict.filter validJump
                        |> Dict.filter notVisited
                        |> Dict.keys
                childDistance = 
                    case Dict.get top board.distances of
                        Just n -> n + 1
                        Nothing -> 1
            in
            findPath { board 
            | next = foldl (\v acc -> Fifo.insert v acc) remain values
            , visited = foldl (\v acc -> Set.insert v acc) board.visited values
            , distances = foldl (\k acc -> Dict.insert k childDistance acc) board.distances values
            }

getValues heights neighbors =
    Dict.filter (\k _ -> List.member k neighbors) heights

score board =
    Dict.get board.start board.distances
    
--------------------------------------------------------------

part2 input =
    input
        |> parseInput
        |> findPath
        |> score2

score2 board =
    let
        aList = Dict.filter (\_ v -> v == 'a' || v == 'S') board.heights
            |> Dict.keys
        values = getValues board.distances aList
            |> Dict.values
    in
    values
        |> List.minimum

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
