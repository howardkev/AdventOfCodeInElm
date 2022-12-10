module GridExample exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import Set exposing (Set)
import List exposing (map, filterMap, foldl, filter, concat, length, range, sort, sum, maximum, sortWith, head, tail)
import String exposing (lines, split, toList)
import Element exposing (row)
import Dict exposing (empty)
import Element exposing (Attribute)
import Fifo

todayDescription : PuzzleDescription
todayDescription = { day = 51, title = "Grid Test" }

sampleInput : String
sampleInput =
    """
#######
#G....#
#.G...#
#.#.#G#
#...#.#
#....G#
#######
"""

type alias Board = Grid MapCell

type MapCell
    = Wall
    | Empty
    | Occupied Unit

type alias Unit =
    { unitType : UnitType
    , hp : Int
    , attack : Int
    }

type UnitType
    = Elf
    | Goblin

toBoard attack grid =
    let
        convert _ v = case v of
            '.' -> Empty
            '#' -> Wall
            'E' -> Occupied { unitType = Elf, hp = 200, attack = attack }
            'G' -> Occupied { unitType = Goblin, hp = 200, attack = 3 }
            _ -> Debug.todo ("invalid char: " ++ String.fromChar v)
    in
        { width = grid.width
        , height = grid.height
        , units = Dict.map convert grid.units }

unitToChar unit = case unit of
    Empty -> '.'
    Wall -> '#'
    Occupied {unitType} -> if unitType == Elf then 'E' else 'G'

displayBoard board =
    Dict.foldl (\_ v a -> (unitToChar v) :: a) [] board.units
    |> List.reverse
    |> LE.groupsOf board.width
    |> map String.fromList
    |> String.join "\n"

parseInput input =
    input
        |> split "\n"
        |> parseGrid
        |> toBoard 3

getUnits board = 
    Dict.filter (\k v -> 
        case v of
            Occupied _ -> True
            _ -> False) board.units

sumHitPoints units =
    List.foldl (\pos total ->
            total + case pos of
                Occupied unit -> unit.hp
                _ -> 0
        ) 0 units

play round board =
    let
        units = getUnits board
        (elves, goblins) = Dict.partition (\k v ->
               case v of
                    Occupied unit -> unit.unitType == Elf
                    _ -> False 
            ) units
    in
        if Dict.size elves == 0 || Dict.size goblins == 0 then
            (round - 1) * sumHitPoints (Dict.values units)
        else
            0

part1 input =
    input
        |> parseInput
        |> play 48
        --|> displayBoard

part2 input =
    input
        |> parseInput

-------------------------------------------------

init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = String.trim sampleInput
        , puzzleInput = Nothing
        , result = Nothing
        , description = todayDescription }, Cmd.none )

toString s =
    if String.startsWith "\"" s then
        String.dropLeft 1 s |> String.dropRight 1
    else
        s

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        p1 = Just (toString (Debug.toString (part1 model.input)))
        p2 = Just (toString (Debug.toString (part2 model.input)))
    in
        mainUpdate msg model sampleInput p1 p2
    
main : Program () Model Msg
main =
  Browser.element 
    { init = init
    , update = update 
    , view = view 
    , subscriptions = subscriptions }
