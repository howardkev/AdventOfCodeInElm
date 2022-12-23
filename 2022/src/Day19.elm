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
    String.trim """
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
"""

type alias Blueprint =
    { id: Int
    , oreRobotCost: Int
    , clayRobotCost: Int
    , obsidianRobotCost: (Int, Int)
    , geodeRobotCost: (Int,Int)
    , maxOre: Int
    , maxClay: Int
    , maxObsidian: Int
    }

type alias Substate = (List Int, List Int, Int)

type alias State = 
    { queue: Fifo Substate
    , best: Int
    }

type alias Cache = Dict Substate Int

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> List.map (solve 24)
        |> List.indexedMap score
        |> List.sum

score : Int -> Int -> Int
score index geodes =
    geodes * (index + 1)

parseInput : String -> List Blueprint
parseInput input =
    input
        |> String.split "\n"
        |> List.map ints
        |> List.filterMap toBlueprints
        
toBlueprints : List Int -> Maybe Blueprint
toBlueprints line =
    case line of
        [a,b,c,d,e,f,g] ->
            let
                maxOre = maximum [ b, c, d, f ] 
                    |> Maybe.withDefault 0
            in
            Just { id = a
                 , oreRobotCost = b
                 , clayRobotCost = c
                 , obsidianRobotCost = (d, e)
                 , geodeRobotCost = (f, g) 
                 , maxOre = maxOre
                 , maxClay = e
                 , maxObsidian = g
                 }
        _ -> Nothing

solve : Int -> Blueprint -> Int
solve minutes bp =
    let
        _ = D.log "solving blueprint" bp.id
        cache = Dict.empty
        queue = Fifo.fromList [([0, 0, 0, 0], [1, 0, 0, 0], minutes)]
    in
    bfs bp cache { queue = queue, best = 0 }
        |> D.log "  geodes"

bfs : Blueprint -> Cache -> State -> Int
bfs bp cache state =
    case Fifo.remove state.queue of
        (Nothing, _) -> 
            state.best
        (Just top, remaining) ->
            let
                best = case top of 
                    (minerals, _, _) ->
                        max state.best (getAt 3 minerals)

                optimized = optimize bp top
                (_, _, t) = optimized
            in
            if t == 0 then
                bfs bp cache { state | queue = remaining, best = best }
            else
                case Dict.get optimized cache of
                    Just n -> 
                        bfs bp cache { state | queue = remaining, best = n }
                    Nothing ->
                        bfs bp (Dict.insert optimized best cache) 
                            { state 
                                | best = best
                                , queue = nextSteps bp optimized
                                    |> foldl Fifo.insert remaining
                            }

optimize : Blueprint -> Substate -> Substate
optimize bp (minerals, robots, t) =
    case (minerals, robots) of
        ([ore, clay, obsidian, geode], [r1, r2, r3, r4]) ->
            let
                r1_ = if r1 > bp.maxOre then bp.maxOre else r1
                r2_ = if r2 > bp.maxClay then bp.maxClay else r2
                r3_ = if r3 > bp.maxObsidian then bp.maxObsidian else r3

                ore_ = if ore >= (t * bp.maxOre - r1_ * (t - 1)) then
                        t * bp.maxOre - r1_ * (t - 1)
                    else
                        ore 

                clay_ = if clay >= (t * bp.maxClay - r2_ * (t - 1)) then
                        t * bp.maxClay - r2_ * (t - 1)
                    else
                        clay
                
                obsidian_ = if obsidian >= (t * bp.maxObsidian - r3_ * (t - 1)) then
                        t * bp.maxObsidian - r3_ * (t - 1)
                    else
                        obsidian
            in
            ([ore_, clay_, obsidian_, geode], [r1_, r2_, r3_, r4], t)
            
        _ ->
            (minerals, robots, t)

nextSteps : Blueprint-> Substate -> List Substate
nextSteps bp (minerals, robots, t) =
    let
        oreRC = bp.oreRobotCost
        clayRC = bp.clayRobotCost
        obsidianRCore = Tuple.first bp.obsidianRobotCost
        obsidianRCclay = Tuple.second bp.obsidianRobotCost
        geodeRCore = Tuple.first bp.geodeRobotCost
        geodeRCobsidian = Tuple.second bp.geodeRobotCost
    in
    case (minerals, robots) of
        ([ore, clay, obsidian, geode], [r1, r2, r3, r4]) ->
                [([ore + r1, clay + r2, obsidian + r3, geode + r4], [r1, r2, r3, r4], t - 1)]
                |> (++) (if ore >= oreRC then
                        [([ore - oreRC + r1, clay + r2, obsidian + r3, geode + r4], [r1 + 1, r2, r3, r4], t - 1)]
                    else
                        [])
                |> (++) (if ore >= clayRC then
                        [([ore - clayRC + r1, clay + r2, obsidian + r3, geode + r4], [r1, r2 + 1, r3, r4], t - 1)]
                    else
                        [])
                |> (++) (if ore >= obsidianRCore && clay >= obsidianRCclay then
                        [([ore - obsidianRCore + r1, clay - obsidianRCclay + r2, obsidian + r3, geode + r4], [r1, r2, r3 + 1, r4], t - 1)]
                    else
                        [])
                |> (++) (if ore >= geodeRCore && obsidian >= geodeRCobsidian then
                        [([ore - geodeRCore + r1, clay + r2, obsidian - geodeRCobsidian + r3, geode + r4], [r1, r2, r3, r4 + 1], t - 1)]
                    else
                        [])
        _ -> []

getAt : Int -> List Int -> Int
getAt i list = 
    LE.getAt i list |> Maybe.withDefault 0

--------------------------------------------------------------

part2 : String -> Int
part2 input =
   input
        |> parseInput
        |> take 3
        |> List.map (solve 32)
        |> List.product

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
