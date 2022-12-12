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
import Fifo exposing (Fifo)
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

type StartingPoint = Start | AllAs
type alias Coordinate = (Int, Int)
type alias Height = Int
type alias Distance = Int

type alias Board = (Coordinate, Coordinate, Dict Coordinate Height)

type alias State = 
    { start : Coordinate
    , end : Coordinate
    , heights : Dict Coordinate Height
    , distances : Dict Coordinate Distance
    , queue : Fifo Coordinate
    }

part1 : String -> Maybe Distance
part1 input =
    input
        |> parseInput
        |> toInitialState Start
        |> findBestPath
        |> score

parseInput : String -> Board
parseInput input =
    input
        |> String.lines
        |> map String.toList
        |> toBoard

toBoard : List (List Char) -> Board
toBoard rows =
    let
        getCode _ v =
            case v of
                'S' -> 1
                'E' -> 26
                n -> Char.toCode n - Char.toCode 'a' + 1
        coordsWithChars = List.indexedMap (\y line -> List.indexedMap (\x value -> ((x, y), value)) line) rows
            |> List.concat
        start = filter (\(_, v) -> v == 'S') coordsWithChars
            |> head
            |> Maybe.map Tuple.first
            |> Maybe.withDefault (0, 0)
        end = filter (\(_, v) -> v == 'E') coordsWithChars
            |> List.head
            |> Maybe.map Tuple.first
            |> Maybe.withDefault (0, 0)
        heights = Dict.fromList coordsWithChars
            |> Dict.map getCode
    in
    (start, end, heights)

toInitialState : StartingPoint -> Board -> State
toInitialState startingPoint (start, end, heights) =
    let
        startPoints = case startingPoint of
            Start -> 
                [start]
            AllAs ->
                Dict.filter (\_ v -> v == 1) heights
                    |> Dict.keys
    in
    { start = start, end = end, heights = heights
    , distances = foldl (\sp dict -> Dict.insert sp 0 dict) Dict.empty startPoints
    , queue = Fifo.fromList startPoints }

findBestPath : State -> State
findBestPath state = 
    case Fifo.remove state.queue of
        (Nothing, _) -> state
        (Just pos, remaining) ->
            if pos == state.end then
                state
            else
                let
                    nextState = calculateOnePos pos remaining state
                in
                findBestPath nextState

calculateOnePos : Coordinate -> Fifo Coordinate -> State -> State
calculateOnePos (x, y) remaining state =
    let
        myValue = Dict.get (x, y) state.heights
            |> Maybe.withDefault 0
        myDist = Dict.get (x, y) state.distances
            |> Maybe.withDefault 0
        adjacent = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        values = Dict.filter (\k _ -> List.member k adjacent) state.heights
            |> Dict.filter (\_ v -> v <= myValue + 1)
            |> Dict.filter (\k _ -> not <| Dict.member k state.distances)
            |> Dict.keys
    in
    { state 
    | queue = foldl (\v q -> Fifo.insert v q) remaining values
    , distances = foldl (\v d -> Dict.insert v (myDist + 1) d) state.distances values
    }

score : State -> Maybe Distance
score state =
    Dict.get state.end state.distances
    
--------------------------------------------------------------

part2 : String -> Maybe Distance
part2 input =
    input
        |> parseInput
        |> toInitialState AllAs
        |> findBestPath
        |> score

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
