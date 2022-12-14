module Day14 exposing (..)

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

todayDescription : PuzzleDescription
todayDescription = { day = 14, title = "---" }

sampleInput : String
sampleInput =
    """
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
"""

type Material = Air | Rock | Sand
type alias Coordinate = (Int, Int)
type alias Grid = Dict Coordinate Material

type alias State = 
    { currentSand : Coordinate
    , grid : Grid
    , bottom : Int
    }

part1 input =
    input
        |> parseInput
        |> toInitialState
        |> play
        |> .grid
        --|> score
        |> printGrid

printGrid grid =
    let
        asList = Dict.toList grid
        bottom = foldl (\((x,y),_) b -> max y b) 0 asList + 1
        top = 0
        right = foldl (\((x,y),_) b -> max x b) 0 asList + 3
        left = foldl (\((x,y),_) b -> min x b) 1000000 asList - 3
        points = pointGrid left top (right - left + 1) (bottom - top + 2)
    in
    displayBoard points grid (right - left + 1) (bottom)

displayBoard points board w bottom =
    let
        unitToChar v =
            case Dict.get v board of
                Just Rock -> '#'
                Just Sand -> 'o'
                _ -> if (Tuple.second v) == bottom then 
                        '-'
                    else
                        ' '
    in
    foldl (\v a -> (unitToChar v) :: a) [] points
        |> List.reverse
        |> LE.groupsOf w
        |> map String.fromList
        |> String.join "\n"

score grid =
    LE.count (\(_, material) -> material == Sand) (Dict.toList grid)

parseInput input =
    input
        |> String.split "\n"
        |> map ints
        |> map (LE.groupsOf 2)
        |> map (filterMap toCoord)
        |> map (LE.groupsOfWithStep 2 1)

toCoord pair =
    case pair of
        [a, b] -> Just (Tuple.pair a b)
        _ -> Nothing

toInitialState rockLines =
    let
        addPoints pts list =
            let
                expandLine (x1,y1) (x2,y2) =
                    if x1 == x2 then
                        if y1 <= y2 then
                            LE.zip (List.repeat 100 x1) (List.range y1 y2)
                        else
                            LE.zip (List.repeat 100 x1) (List.range y2 y1)
                    else
                        if x1 <= x2 then
                            LE.zip  (List.range x1 x2) (List.repeat 100 y1)
                        else
                            LE.zip (List.range x2 x1) (List.repeat 100 y1) 

                toAdd = case pts of
                    [x,y] ->
                        foldl (\p l -> (p, Rock) :: l) [] (expandLine x y)
                    _ -> []
            in
            list ++ toAdd
        allRocks = foldl (\line list ->
            foldl (\p l -> addPoints p l) list line)
            [] rockLines
        bottom = foldl (\((x,y),_) b -> max y b) 0 allRocks
    in
    { currentSand = (500, 0)
    , grid = Dict.fromList allRocks
    , bottom = bottom + 2
    }

play state =
    if Tuple.second state.currentSand == state.bottom then
        state
    else
        let
            (x, y) = state.currentSand
            nextPos = 
                if Dict.member (x, y + 1) state.grid then
                    if Dict.member (x - 1, y + 1) state.grid then
                        if Dict.member (x + 1, y + 1) state.grid then
                            (500, 0)
                        else
                            (x + 1, y + 1)
                    else
                        (x - 1, y + 1)
                else
                    (x, y + 1)
            nextGrid = 
                if nextPos == (500, 0) then
                    Dict.insert (x, y) Sand state.grid
                else
                    state.grid
        in
        play { state 
        | currentSand = nextPos
        , grid = nextGrid
        }

--------------------------------------------------------------

part2 input =
   input
        |> parseInput
        |> toInitialState
        |> play2
        |> .grid
        --|> score
        |> printGrid

play2 state =
    let
        (x, y) = state.currentSand
        isBlocked (x1, y1) =
            Dict.member (x1, y1) state.grid ||
                y1 == state.bottom
        nextPos = 
            if isBlocked (x, y + 1) then
                if isBlocked (x - 1, y + 1) then
                    if isBlocked (x + 1, y + 1) then
                        (500, 0)
                    else
                        (x + 1, y + 1)
                else
                    (x - 1, y + 1)
            else
                (x, y + 1)
        nextGrid = 
            if nextPos == (500, 0) then
                Dict.insert (x, y) Sand state.grid
            else
                state.grid
        finished = (x, y) == (500, 0) &&
            isBlocked (x, y + 1) &&
            isBlocked (x - 1, y + 1) &&
            isBlocked (x + 1, y + 1)
    in
    if finished then
        { state 
        | grid = Dict.insert (x, y) Sand state.grid
        }
    else
        play2 { state 
        | currentSand = nextPos
        , grid = nextGrid
        }

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
