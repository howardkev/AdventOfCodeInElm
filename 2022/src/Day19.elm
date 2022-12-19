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
import Day10 exposing (State)

todayDescription : PuzzleDescription
todayDescription = { day = 19, title = "---" }

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

type alias State =
    { minute : Int
    , oreRobots : Int
    , clayRobots : Int
    , obsidianRobots : Int
    , geodeRobots : Int
    , ore : Int
    , clay : Int
    , obsidian : Int
    , geode : Int
    }

part1 input =
    input
        |> parseInput
        --|> take 1
        |> map calculate
        |> List.indexedMap score

score index { geode } =
    geode * (index + 1)
        |> D.log "oneScore"

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

calculate : Blueprint -> State
calculate blueprint =
    let
        _ = D.log "running blueprint" blueprint.id
    in
    step Dict.empty blueprint
        { minute = 0
        , oreRobots = 1
        , clayRobots = 0
        , obsidianRobots = 0
        , geodeRobots = 0
        , ore = 0
        , clay = 0
        , obsidian = 0
        , geode = 0
        }

--step : Blueprint -> State -> State
step cache blueprint state =
    if state.minute == 3 then
        state
    else
        let
            newMaterials = 
                { state 
                    | ore = state.ore + state.oreRobots 
                    , clay = state.clay + state.clayRobots
                    , obsidian = state.obsidian + state.obsidianRobots
                    , geode = state.geode + state.geodeRobots
                    , minute = state.minute + 1
                }

            justWait =
                Just newMaterials

            createOre = 
                if state.ore >= blueprint.ore then
                    Just {
                        newMaterials
                            | ore = newMaterials.ore - blueprint.ore
                            , oreRobots = state.oreRobots + 1
                    }
                else
                    Nothing

            createClay = 
                if state.ore >= blueprint.clay then
                    Just {
                        newMaterials
                            | ore = newMaterials.ore - blueprint.clay
                            , clayRobots = state.clayRobots + 1
                    }
                else
                    Nothing

            createObsidian =
                let
                    (ore, clay) = blueprint.obsidian
                in
                if state.ore >= ore && state.clay >= clay then
                    Just {
                        newMaterials
                            | ore = newMaterials.ore - ore
                            , clay = newMaterials.clay - clay
                            , obsidianRobots = state.obsidianRobots + 1
                    }
                else
                    Nothing

            createGeode =
                let
                    (ore, obsidian) = blueprint.geode
                in
                if state.ore >= ore && state.obsidian >= obsidian then
                    Just {
                        newMaterials
                            | ore = newMaterials.ore - ore
                            , obsidian = newMaterials.obsidian - obsidian
                            , geodeRobots = state.geodeRobots + 1
                    }
                else
                    Nothing

            nextStates = filterMap identity [justWait, createOre, createClay, createObsidian, createGeode]
                --|> D.log "nextStates"
            in
            pickBest (foldl (\next (csh, lst) -> 
                let
                    key = [next.minute, next.oreRobots, next.clayRobots
                        , next.obsidianRobots, next.geodeRobots
                        , next.ore, next.clay, next.obsidian, next.geode]
                in
                case Dict.get key csh of
                    Just n -> 
                        (csh, n::lst)
                    _ ->
                        let
                            value = step csh blueprint next
                            c = Dict.insert key value csh
                        in
                        (c, value::lst)
            ) (cache, []) nextStates)

pickBest : (a, List State) -> State
pickBest (_, list) =
    case LE.maximumBy .geode list of
        Just n -> n
        _ -> D.todo "no max"

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
