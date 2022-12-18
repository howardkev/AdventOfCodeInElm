module Day18 exposing (..)

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
todayDescription = { day = 18, title = "Boiling Boulders" }

sampleInput : String
sampleInput =
    """
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
"""

type alias Coordinate = (Int, Int, Int)
type alias Cubes = Set Coordinate

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> solve

parseInput : String -> Cubes
parseInput input =
    input
        |> String.split "\n"
        |> map ints
        |> filterMap toCubes
        |> Set.fromList

toCubes : List Int -> Maybe Coordinate
toCubes lines =
    case lines of
        [x, y, z] -> Just (x, y, z)
        _ -> Nothing

solve : Cubes -> Int
solve cubes =
    let
        solveHelper : Coordinate -> Int -> Int
        solveHelper pos ans =
            let
                neighborCount = getNeighbors pos cubes
                    |> Set.size
            in
            ans + 6 - neighborCount
    in
    Set.foldl solveHelper 0 cubes

getNeighbors : Coordinate -> Cubes -> Set Coordinate
getNeighbors (x, y, z) cubes =
    [ (x + 1, y, z), (x - 1, y, z), (x, y + 1, z)
    , (x, y - 1, z), (x, y, z + 1), (x, y, z - 1) ]
    |> filter (\friend -> Set.member friend cubes)
    |> Set.fromList

--------------------------------------------------------------

type alias BFS = Fifo Coordinate

part2 : String -> Int
part2 input =
   input
        |> parseInput
        |> solve2

solve2 : Cubes -> Int
solve2 cubes =
    let
        cubeTotalPerimeter = solve cubes
        internal = solve (getInternal cubes)
    in
    cubeTotalPerimeter - internal

getInternal : Cubes -> Cubes
getInternal cubes =
    let
        (x1, x2) = Set.foldl (\(x,_,_) (mn, mx) ->
                (min mn x, max mx x)
            ) (100000, 0) cubes
        (y1, y2) = Set.foldl (\(_,y,_) (mn, mx) ->
                (min mn y, max mx y)
            ) (100000, 0) cubes
        (z1, z2) = Set.foldl (\(_,_,z) (mn, mx) ->
                (min mn z, max mx z)
            ) (100000, 0) cubes

        containingCube =
            let
                xs = List.range (x1 - 1) (x2 + 1)
                ys = List.range (y1 - 1) (y2 + 1)
                zs = List.range (z1 - 1) (z2 + 1)
            in
            zs |> List.concatMap (\z_ -> 
                ys |> List.concatMap (\y_ -> 
                    xs |> List.concatMap (\x_ -> [(x_, y_, z_)])))

        possible = Set.diff (Set.fromList containingCube) cubes
    in
    removeEdges (Fifo.fromList [(x1,y1,z1)]) possible

removeEdges : BFS -> Cubes -> Cubes
removeEdges queue state =
    case Fifo.remove queue of
        (Nothing, _) -> state
        (Just top, remaining) ->
            let
                (nextQ, nextState) = removeOne top remaining state
            in
            removeEdges nextQ nextState

removeOne : Coordinate -> BFS -> Cubes -> (BFS, Cubes)
removeOne pos remaining state =
    let
        neighbors = getNeighbors pos state 
    in
    ( Set.foldl Fifo.insert remaining neighbors
    , Set.foldl Set.remove state (Set.insert pos neighbors) )

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
