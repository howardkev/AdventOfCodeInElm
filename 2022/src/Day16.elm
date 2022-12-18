module Day16 exposing (..)

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
todayDescription = { day = 16, title = "Proboscidea Volcanium" }

sampleInput : String
sampleInput =
    """
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"""

part1 input =
    input
        |> parseInput
        |> toInitialState
        |> getPaths
        |> .results
        |> maximum

parseInput input =
    input
        |> String.split "\n"
        |> filterMap parseValve

parseValve line =
    case String.words line of
        [_, valve, _, _, rate, _, _, _, _, v1] ->
            Just (valve, toRate rate, [v1])
        [_, valve, _, _, rate, _, _, _, _, v1, v2] ->
            Just (valve, toRate rate, [toValve v1, v2])
        [_, valve, _, _, rate, _, _, _, _, v1, v2, v3] ->
            Just (valve, toRate rate, [toValve v1, toValve v2, v3])
        [_, valve, _, _, rate, _, _, _, _, v1, v2, v3, v4] ->
            Just (valve, toRate rate, [toValve v1, toValve v2, toValve v3, v4])
        [_, valve, _, _, rate, _, _, _, _, v1, v2, v3, v4, v5] ->
            Just (valve, toRate rate, [toValve v1, toValve v2, toValve v3, toValve v4, v5])
        _ -> Nothing

toValve v =
    String.toList v |> take 2 |> String.fromList

toRate r =
    head (ints r) |> Maybe.withDefault 0 

toInitialState valves =
    let
        flows = foldl (\(v, flow, _) dict -> Dict.insert v flow dict ) Dict.empty valves
        adj = foldl (\(v, _, children) dict -> Dict.insert v children dict ) Dict.empty valves
        targets = foldl (\(v, flow, _) set ->
            if flow > 0 then
                Set.insert v set 
            else
                set ) Set.empty valves
    in
    { flows = flows
    , adj = adj
    , results = []
    , stack = [{ node = "AA", score = 0, minutes = 0, targets = targets }]
    }

getPaths state =
    case state.stack of
        [] -> state
        top :: remaining ->
            let
                nextMoves = doNextMove top state
            in
            if List.length nextMoves == 0 then
                getPaths { state 
                        | stack = remaining
                        , results = top.score :: state.results 
                    }
            else
                getPaths { state | stack = nextMoves ++ remaining }

doNextMove top state =
    let
        targetMin = 30
        scores = getScores top.node state top.targets (30 - top.minutes)
            |> filter (\(n,s,m) -> top.minutes + m <= targetMin)
        moves = foldl (\(n,s,m) list -> 
                { node = n
                 , score = top.score + s
                 , minutes = top.minutes + m
                 , targets = Set.remove n top.targets } :: list
            ) [] scores
    in
    moves

getScores node state targets minutes =
    let
        distances = distanceFrom node state targets
        scores = map (\dist ->
                let
                    (n, d) = dist
                    flow = (Dict.get n state.flows |> Maybe.withDefault 0)
                    score = (minutes - d - 1) * flow
                in
                (n, score, d + 1)
            ) (Dict.toList distances)
    in
    scores

distanceFrom node mainState targets =
    let
        calcNext top remaining state =
            let
                myDist = Dict.get top state.distances
                    |> Maybe.withDefault 0
                adj = Dict.get top mainState.adj |> Maybe.withDefault []
                 |> filter (\n -> not (Dict.member n state.distances))
            in
            { state 
                | distances = foldl (\v d -> Dict.insert v (myDist + 1) d) state.distances adj
                , queue = foldl (\v q -> Fifo.insert v q) remaining adj
            }
        helper state = 
            case Fifo.remove state.queue of
                (Nothing, _) -> state
                (Just top, remaining) ->
                    let
                        nextState = calcNext top remaining state
                    in
                    helper nextState
        x = helper { queue = Fifo.fromList [node]
            , distances = Dict.fromList [(node, 0)]
            }
    in
    x 
        |> .distances 
        |> Dict.filter (\k _ -> Set.member k targets)


--------------------------------------------------------------

part2 input =
   input
        |> parseInput
        |> toInitialState2
        |> getPaths2
        |> .result
        --|> maximum

toInitialState2 valves =
    let
        flows = foldl (\(v, flow, _) dict -> Dict.insert v flow dict ) Dict.empty valves
        adj = foldl (\(v, _, children) dict -> Dict.insert v children dict ) Dict.empty valves
        targets = foldl (\(v, flow, _) set ->
            if flow > 0 then
                Set.insert v set 
            else
                set ) Set.empty valves
    in
    { flows = flows
    , adj = adj
    , result = 0
    , stack = [
        { me = { node = "AA", minutes = 0 }
        , elephant = { node = "AA", minutes = 0 }
        , score = 0
        , targets = targets
        }
        ]
    }

getPaths2 state =
    case state.stack of
        [] -> state
        top :: remaining ->
            let
                nextMoves = doNextMove2 top state
            in
            if List.length nextMoves == 0 then
                let
                    result = 
                        if top.score > state.result then
                            let
                                _ = Debug.log "result" top.score
                            in
                            top.score
                        else
                            state.result
                in
                getPaths2 { state 
                        | stack = remaining
                        , result = result
                    }
            else
                getPaths2 { state | stack = nextMoves ++ remaining }

doNextMove2 top state =
    let
        targetMin = 26
        myScores = getScores top.me.node state top.targets (26 - top.me.minutes)
            |> filter (\(n,s,m) -> top.me.minutes + m <= targetMin)

        eleScores = getScores top.elephant.node state top.targets (26 - top.elephant.minutes)
            |> filter (\(n,s,m) -> top.elephant.minutes + m <= targetMin)

        moves = 
            if List.isEmpty eleScores then
                foldl (\(n1,s1,m1) list -> 
                        { me = { node = n1, minutes = top.me.minutes + m1 }
                        , elephant = { node = n1, minutes = 1000 }
                        , score = top.score + s1
                        , targets = Set.remove n1 top.targets
                        } :: list
                    ) [] myScores
            else if List.isEmpty myScores then
                foldl (\(n1,s1,m1) list -> 
                        { me = { node = n1, minutes = 1000 }
                        , elephant = { node = n1, minutes = top.elephant.minutes + m1 }
                        , score = top.score + s1
                        , targets = Set.remove n1 top.targets
                        } :: list
                    ) [] eleScores
            else
                let
                    pairs = getAllPairs myScores eleScores
                        |> filter (\((n1,_,_),(n2,_,_)) -> n1 /= n2)
                in
                foldl (\((n1,s1,m1),(n2,s2,m2)) list -> 
                        { me = { node = n1, minutes = top.me.minutes + m1 }
                        , elephant = { node = n2, minutes = top.elephant.minutes + m2 }
                        , score = top.score + s1 + s2
                        , targets = Set.remove n1 top.targets
                            |> Set.remove n2 
                        } :: list
                    ) [] pairs
    in
    moves

getAllPairs list1 list2 =
    List.concatMap (\score ->
            foldl (\ele list -> (score, ele) :: list) [] list2
        )  list1

-- 1850
-- 1945 - low
-- 2200 - low
-- 2300 - X
-- 2350 - X
-- 2500 - high

--2214
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
