module Day24 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import List exposing (map, filterMap, foldl, take, filter, concat, length, range, sort, maximum, sortWith, head, tail, any, all, reverse)
import Parser exposing (Parser, Trailing(..))
import Debug as D
import Fifo exposing (Fifo)
import Set exposing (Set)

todayDescription : PuzzleDescription
todayDescription = { day = 24, title = "Blizzard Basin" }

sampleInput : String
sampleInput =
    String.trim """
#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
"""

type alias Coordinate = (Int, Int)
type alias Grid = Dict Coordinate Tile

type Tile
    = Wall
    | Ground
    | Blizzard Direction
    | Multiple (List Direction)
    | Expedition

type Direction
    = Up
    | Down
    | Left
    | Right

part1 input =
    input
        |> parseInput
        |> solve findBestPath
        --|> combineExpedition
        --|> printGrid tileToChar

combineExpedition state =
    Dict.union (Dict.singleton state.position Expedition) state.grid

parseInput input =
    input
        |> String.split "\n"
        |> parseGrid charToTile

charToTile : Char -> Maybe Tile
charToTile c = 
    case c of
        '#' -> Just Wall
        '.' -> Just Ground
        '>' -> Just (Blizzard Right)
        '<' -> Just (Blizzard Left)
        '^' -> Just (Blizzard Up)
        'v' -> Just (Blizzard Down)
        _ -> Nothing

tileToChar : Coordinate -> Grid -> Char
tileToChar v grid =
    case Dict.get v grid of
        Just Wall -> '#'
        Just Ground -> '.'
        Just (Blizzard Right) -> '>'
        Just (Blizzard Left) -> '<'
        Just (Blizzard Up) -> '^'
        Just (Blizzard Down) -> 'v'
        Just (Multiple n) -> Char.fromCode ((Char.toCode '0') + length n)
        Just Expedition -> 'E'
        Nothing -> '.'

getMaxX : Grid -> Int
getMaxX grid =
    Dict.keys grid 
        |> foldl (\(x,_) acc -> max acc x) 0

getMaxY : Grid -> Int
getMaxY grid =
    Dict.keys grid 
        |> foldl (\(_,y) acc -> max acc y) 0

solve fn grid =
    let
        queue = Fifo.fromList [((1, 0), 0, 0)]
        maxX = getMaxX grid
        maxY = getMaxY grid
        (_, grids) = foldl (\index (g,d) -> 
                let
                    nextGrid = getNextGrid maxX maxY g
                in
                (nextGrid, Dict.insert index nextGrid d)
            ) (grid, Dict.empty) (range 0 1000)

        state = { end = (maxX - 1, maxY)
                , maxX = maxX
                , maxY = maxY
                , queue = queue
                , grids = grids
                , cache = Set.empty }
    in
    fn state

findBestPath state = 
    case Fifo.remove state.queue of
        (Nothing, _) -> 0
        (Just ((pos, minutes, _) as top), remaining) ->
            if pos == state.end then
                minutes + 1
            else
                let
                    nextState = calculateOnePos top remaining state
                in
                findBestPath nextState

calculateOnePos top remaining state =
    let
        (_, minutes, quest) = top

        possibleMoves = oneRound top state

        nextQList = 
            map (\pos ->
                    (pos, minutes + 1, quest)
                ) possibleMoves
            |> filter (\item -> 
                not <| Set.member item state.cache)

        newCache = foldl Set.insert state.cache nextQList
        
    in
    { state 
        | queue = foldl Fifo.insert remaining nextQList
        , cache = newCache
    }

getNextGrid maxX maxY grid =
    let
        isWall (x, y) = case Dict.get (x, y) grid of
            Just Wall -> True
            _ -> False

        maybeUpdate value current =
            case value of
                (Blizzard newDirection) ->
                    case current of
                        Just (Blizzard direction) -> 
                            Just (Multiple [newDirection, direction])
                        Just (Multiple directions) ->
                            Just (Multiple (newDirection::directions))
                        _ -> 
                            Just value
                _ -> D.todo "bad update"

        moveBlizzard (x, y) blizzard dict =
            case blizzard of
                Blizzard Right -> 
                    (if isWall (x + 1, y) then
                        Dict.update (1, y) (maybeUpdate blizzard) dict
                    else
                        Dict.update (x + 1, y) (maybeUpdate blizzard) dict)
                Blizzard Down -> 
                    (if isWall (x, y + 1) then
                        Dict.update (x, 1) (maybeUpdate blizzard) dict
                    else
                        Dict.update (x, y + 1) (maybeUpdate blizzard) dict)
                Blizzard Up -> 
                    (if isWall (x, y - 1) then
                        Dict.update (x, maxY - 1) (maybeUpdate blizzard) dict
                    else
                        Dict.update (x, y - 1) (maybeUpdate blizzard) dict)
                Blizzard Left -> 
                    (if isWall (x - 1, y) then
                        Dict.update (maxX - 1, y) (maybeUpdate blizzard) dict
                    else
                        Dict.update (x - 1, y) (maybeUpdate blizzard) dict)
                _ -> dict

        blizzardMoves = Dict.foldl (\((x, y) as pos) v dict ->
                case v of
                    Blizzard _ ->
                        moveBlizzard (x, y) v dict
                    Multiple directions ->
                        directions
                            |> map (\dir -> Blizzard dir)
                            |> foldl (moveBlizzard pos) dict
                    Ground ->
                        dict
                    _ -> Dict.insert pos v dict
            ) Dict.empty grid
    in
    blizzardMoves

oneRound (expeditionPos, minutes, _) state =
    let
        grid = 
            case Dict.get (minutes + 1) state.grids of
                Just n -> n
                _ -> D.todo "missing grid"

        moveExpedition =
            let
                (x,y) = expeditionPos
                deltas = [(0,0), (1,0), (-1,0), (0,1), (0,-1)]
                possibleMoves =
                    filterMap (\(dx, dy) -> 
                            case Dict.get (x + dx, y + dy) grid of
                                Just (Blizzard _) -> Nothing
                                Just Wall -> Nothing
                                Just (Multiple _) -> Nothing
                                _ -> Just (x + dx, y + dy)
                        ) deltas
                    |> filter (\(x1, y1) -> 
                        x1 > 0 && y1 > -1)
            in
            possibleMoves
    in
    moveExpedition

--------------------------------------------------------------

part2 input =
    input
        |> parseInput
        |> solve findBestPath2

findBestPath2 state = 
    case Fifo.remove state.queue of
        (Nothing, _) -> 0
        (Just ((pos, minutes, quest) as top), remaining) ->
            if quest == 0 && pos == state.end then
                findBestPath2 { state | queue = Fifo.fromList [(pos, minutes, 1)]}
            else if quest == 1 && pos == (1, 0) then
                findBestPath2 { state | queue = Fifo.fromList [(pos, minutes, 2)]}
            else if quest == 2 && pos == state.end then
                minutes + 1
            else
                let
                    nextState = calculateOnePos top remaining state
                in
                findBestPath2 nextState

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
