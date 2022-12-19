module Day19 exposing (..)

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
import Debug as D
import Array exposing (Array)

todayDescription : PuzzleDescription
todayDescription = { day = 19, title = "Not Enough Minerals" }

sampleInput : String
sampleInput =
    """
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
"""
type alias Blueprint =
    { id: Int
    , ore: Int
    , clay: Int
    , obsidian: (Int, Int)
    , geode: (Int,Int)
    }

part1 input =
    input
        |> parseInput
        |> take 1
        |> map solve

parseInput input =
    input
        |> String.split "\n"
        |> map ints
        |> filterMap toBlueprints

toBlueprints : List Int -> Maybe Blueprint
toBlueprints line =
    case line of
        [a,b,c,d,e,f,g] ->
            Just { id = a, ore = b, clay = c, obsidian = (d,e), geode = (f,g)}
        _ -> Nothing

solve blueprint =
    let
        _ = D.log "running blueprint" blueprint.id
        cache = Dict.empty
        minutes = 25
        robots = [1, 0, 0, 0]
        minerals = [0, 0, 0, 0]
    in
    dfs blueprint cache minutes robots minerals
        |> Tuple.second
    
type alias Key = (Int, List Int, List Int)
type alias Cache = Dict Key Int

dfs : Blueprint -> Cache -> Int -> List Int -> List Int -> (Cache, Int)
dfs bp cache minutes robots minerals =
    let
        key = (minutes, robots, minerals)
        getMineralAt index list = LE.getAt index list |> Maybe.withDefault 0
    in
    if minutes == 0 then
        (cache, getMineralAt 3 minerals)
    else
        case Dict.get key cache of
            Just n -> (cache, n)
            Nothing ->
                let
                    newMinerals = List.map2 (\mineral bot ->
                            mineral + bot
                        ) minerals robots

                    buyNothing = Just (newMinerals, robots)

                    createOre = 
                        if getMineralAt 0 minerals >= bp.ore then
                            Just 
                                ( List.indexedMap (\i mineral ->
                                        if i == 0 then
                                            mineral - bp.ore
                                        else
                                            mineral
                                    ) newMinerals
                                , List.indexedMap (\i bot ->
                                        if i == 0 then
                                            bot + 1
                                        else
                                            bot
                                    ) robots
                                )
                        else
                            Nothing

                    createClay = 
                        if getMineralAt 0 minerals >= bp.clay then
                            Just 
                                ( List.indexedMap (\i mineral ->
                                        if i == 0 then
                                            mineral - bp.clay
                                        else
                                            mineral
                                    ) newMinerals
                                , List.indexedMap (\i bot ->
                                        if i == 1 then
                                            bot + 1
                                        else
                                            bot
                                    ) robots
                                )
                        else
                            Nothing

                    createObsidian = 
                        let
                            (ore, clay) = bp.obsidian
                        in
                        if (getMineralAt 0 minerals >= ore &&
                            getMineralAt 1 minerals >= clay) then
                            Just 
                                ( List.indexedMap (\i mineral ->
                                        if i == 0 then
                                            mineral - ore
                                        else if i == 1 then
                                            mineral - clay
                                        else
                                            mineral
                                    ) newMinerals
                                , List.indexedMap (\i bot ->
                                        if i == 2 then
                                            bot + 1
                                        else
                                            bot
                                    ) robots
                                )
                        else
                            Nothing

                    createGeode = 
                        let
                            (ore, obsidian) = bp.geode
                        in
                        if (getMineralAt 0 minerals >= ore &&
                            getMineralAt 2 minerals >= obsidian) then
                            Just 
                                ( List.indexedMap (\i mineral ->
                                        if i == 0 then
                                            mineral - ore
                                        else if i == 2 then
                                            mineral - obsidian
                                        else
                                            mineral
                                    ) newMinerals
                                , List.indexedMap (\i bot ->
                                        if i == 3 then
                                            bot + 1
                                        else
                                            bot
                                    ) robots
                                )
                        else
                            Nothing

                    nextStates = filterMap identity [buyNothing, createOre, createClay, createObsidian, createGeode]

                    (c2, geodesList) = foldl (\(m, bots) (csh, lst) -> 
                            let
                                (c1, value) = dfs bp csh (minutes - 1) bots m
                            in
                            (c1, value::lst)
                        ) (cache, []) nextStates
                        --|> D.log "results"

                    bestGeode = maximum geodesList
                        |> Maybe.withDefault 0

                    c3 = Dict.insert key bestGeode c2
                in
                (c3, bestGeode)

--------------------------------------------------------------

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
