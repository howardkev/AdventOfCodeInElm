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

type alias Key = (Int, List Int, List Int)
type alias Cache = Dict Key Int

type alias Blueprint =
    { id: Int
    , ore: Int
    , clay: Int
    , obsidian: (Int, Int)
    , geode: (Int,Int)
    , maxSpend: List Int
    , recipes: List (List (Int, Int))
    }

part1 input =
    input
        |> parseInput
        |> take 1
        |> List.map solve

parseInput input =
    input
        |> String.split "\n"
        |> List.map ints
        |> List.filterMap toBlueprints
        
toBlueprints : List Int -> Maybe Blueprint
toBlueprints line =
    case line of
        [a,b,c,d,e,f,g] ->
            Just { id = a, ore = b, clay = c, obsidian = (d,e), geode = (f,g)
                , maxSpend = [ max (max b c) (max d f), e, g]
                , recipes = [[(b, 0)], [(c, 0)], [(d, 0), (e, 1)], [(f, 0), (g, 2)]]
                }
        _ -> Nothing

solve blueprint =
    let
        cache = Dict.empty
        minutes = 24
        robots = [1, 0, 0, 0]
        minerals = [0, 0, 0, 0]
    in
    dfs blueprint cache minutes robots minerals
    
dfs : Blueprint -> Cache -> Int -> List Int -> List Int -> (Cache, Int)
dfs bp cache minutes robots minerals =
    let
        key = (minutes, robots, minerals)
        getAt i list = LE.getAt i list |> Maybe.withDefault 0
    in
    if minutes <= 20 then
        --let
        --    _ = Debug.log "reject" (minutes, robots, minerals)
        --in
        (cache, getAt 3 minerals)
    else
        case Dict.get key cache of
            Just n -> (cache, n)
            Nothing ->
                let
                    --_ = Debug.log "here" (minutes, robots, minerals)
                    maxVal = (getAt 3 minerals) + (getAt 3 robots) * minutes
                        
                    toCheck = List.indexedMap (\robot recipe ->
                            if robot /= 3 && getAt robot robots >= getAt robot bp.maxSpend then
                                Nothing
                            else
                                Just (robot, recipe)
                        ) bp.recipes
                        |> List.filterMap identity
                        
                    hasNothing list =
                        List.any (\a -> 
                                case a of
                                    Nothing -> True
                                    _ -> False
                            ) list
                            
                    waitCalculation list =
                        list
                            |> List.map (\(amount, rtype) ->
                                    if getAt rtype robots == 0 then
                                        Nothing
                                    else
                                        Just ((amount - (getAt rtype minerals)) // getAt rtype robots)
                                )
                        
                    waits = toCheck
                        |> List.map (\(a, b) -> 
                                (a, waitCalculation b)
                            )
                        |> List.filterMap (\(a, b) ->
                                if hasNothing b then
                                    Nothing
                                else
                                    Just (a, List.filterMap identity b)
                            )
                        |> List.map (\(a, b) -> 
                                (a, List.maximum b |> Maybe.withDefault 0)
                            )
                        
                    remaining = List.map (\(a, wait) ->
                            (a, minutes - wait - 1, wait)
                        ) waits
                        |> List.filter (\(_, b, _) -> b > 0)
                        
                    calculateMineralsH (bot,remain,wait) = 
                        let
                            --_ = Debug.log "zip" (bot,remain,wait)
                            zip = LE.zip minerals robots
                            recipe = LE.getAt bot bp.recipes |> Maybe.withDefault [(0,0)]
                            amounts = List.map (\(x, y) -> x + y * (wait + 1)) zip
                            adjusted = List.foldl (\(ramt, rtype) (amts, bots) ->
                                    let
                                        --_ = Debug.log "bots" (bots, bot, newBots)
                                        newBots = List.indexedMap (\i b ->
                                                if i == bot then
                                                    b + 1
                                                else
                                                    b
                                            ) bots
                                        currentAmt = LE.getAt rtype amts |> Maybe.withDefault 0
                                        newAmounts = 
                                            LE.setAt rtype (min (currentAmt - ramt) (
                                                    remain * (getAt rtype bp.maxSpend)
                                                )) amts
                                    in
                                    ( newAmounts, newBots)
                                ) (amounts, robots) recipe
                        in
                        ((bot,remain,wait), adjusted)
                        
                    nextRuns = remaining
                        |> List.map calculateMineralsH
                        
                    (c2, geodesList) = List.foldl (\((bot, remain, wait), (amounts, bots)) (csh, lst) ->
                            let
                                --_ = Debug.log "inside" (minutes, (remain, bots, amounts))
                                (c1, value) = dfs bp csh remain bots amounts
                            in
                            (c1, value::lst)
                        ) (cache, []) nextRuns
                    
                    bestGeode = maximum (maxVal::geodesList)
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
