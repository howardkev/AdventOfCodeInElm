module Day8 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Dict exposing (Dict)
import List exposing (map, filterMap, indexedMap, reverse, concat, length, range, maximum, product, any, all)

todayDescription : PuzzleDescription
todayDescription = { day = 8, title = "Treetop Tree House" }

sampleInput : String
sampleInput =
    """
30373
25512
65332
33549
35390
"""

type alias Trees = Dict Coordinate Height
type alias Coordinate = (Int, Int)
type alias Height = Int

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> calculateVisibleTrees

parseInput : String -> Trees
parseInput input =
    input
        |> String.lines
        |> map (String.split "" >> filterMap String.toInt)
        |> toCoords
        |> concat
        |> Dict.fromList

toCoords : List (List Height) -> List (List (Coordinate, Height))
toCoords rows =
    List.indexedMap (\y line -> indexedMap (\x value -> ((x, y), value)) line) rows

calculateVisibleTrees : Trees -> Int
calculateVisibleTrees trees =
    let
        positions = Dict.keys trees
        maxX = map Tuple.first positions |> maximum |> Maybe.withDefault 0
        maxY = map Tuple.second positions |> maximum |> Maybe.withDefault 0
        
        isVisible : Coordinate -> Bool
        isVisible pos =
            let
                myHeight = Dict.get pos trees |> Maybe.withDefault 0
                neighbors = getNeighbors pos maxX maxY trees
            in
            neighbors
                |> any (all (isLessThan myHeight))
    in
    LE.count isVisible positions

getNeighbors : Coordinate -> Int -> Int -> Trees -> List (List Height)
getNeighbors (x, y) maxX maxY trees = 
    let
        left = range 0 (x - 1) |> reverse
            |> filterMap (\n -> Dict.get (n, y) trees)
        right = range (x + 1) maxX
            |> filterMap (\n -> Dict.get (n, y) trees)
        top = range 0 (y - 1) |> List.reverse
            |> filterMap (\n -> Dict.get (x, n) trees)
        bottom = range (y + 1) maxY
            |> filterMap (\n -> Dict.get (x, n) trees)
    in
    [left, right, top, bottom]
      
isLessThan : comparable -> comparable -> Bool
isLessThan a b = b < a

----------------------------------------------------------

part2 : String -> Maybe Int
part2 input =
    input
        |> parseInput
        |> calculateScenicScore

calculateScenicScore : Trees -> Maybe Int
calculateScenicScore trees =
    let
        positions = Dict.keys trees
        maxX = map Tuple.first positions |> maximum |> Maybe.withDefault 0
        maxY = map Tuple.second positions |> maximum |> Maybe.withDefault 0
        
        getViewingDistance : Coordinate -> Int
        getViewingDistance pos =
            let
                myHeight = Dict.get pos trees |> Maybe.withDefault 0
                neighbors = getNeighbors pos maxX maxY trees
            in
            neighbors
                |> map (getOneViewDistance myHeight)
                |> product
    in
    map getViewingDistance positions
        |> maximum

getOneViewDistance : Height -> List Height -> Int
getOneViewDistance myHeight dir =
    length (takeUntil (\h -> h >= myHeight) dir)

takeUntil : (a -> Bool) -> List a -> List a
takeUntil predicate =
    let
        takeUntilHelper acc list =
            case list of
                [] -> reverse acc
                x :: xs ->
                    if predicate x then
                        reverse (x :: acc)
                    else
                        takeUntilHelper (x :: acc) xs
    in
    takeUntilHelper []

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
