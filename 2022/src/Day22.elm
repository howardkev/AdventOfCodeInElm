module Day22 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import List exposing (map, filterMap, foldl, take, filter, concat, length, range, sort, maximum, sortWith, head, tail, any, all, reverse)
import Parser exposing (Parser, Trailing(..))
import Debug as D

todayDescription : PuzzleDescription
todayDescription = { day = 22, title = "Monkey Map" }

sampleInput : String
sampleInput =
    """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"""

type alias Coordinate = (Int, Int)
type alias BlockId = Int
type alias Location = (Coordinate, Direction)

type Command
    = TurnRight
    | TurnLeft
    | Move Int

type Tile
    = Wall
    | Floor
    | Empty

type Direction
    = Left
    | Up
    | Right
    | Down

type alias Grid = Dict Coordinate Tile
type alias Connection = (Direction, BlockId, Direction)
type alias Connections = Dict BlockId (List Connection)

type alias State = 
    { grid: Grid
    , commands: List Command
    , blockSize: Int
    , positions: Dict BlockId { left:Int, top:Int }
    , connections: Connections
    }

part1 : String -> Maybe Int
part1 input =
    input
        |> parseInput 
        |> Maybe.map (toInitialState connectionsForExample1 connectionsForPuzzle1)
        |> Maybe.map solve
        |> Maybe.map score

score : Location -> Int
score ((x,y), direction) =
    1000 * (y + 1) + 4 * (x + 1) + case direction of
        Right -> 0
        Down -> 1
        Left -> 2
        Up -> 3

parseInput : String -> Maybe (Grid, List Command)
parseInput input =
    input
        |> String.split "\n\n"
        |> parseBoth

parseBoth : List String -> Maybe (Grid, List Command)
parseBoth groups =
    case groups of
        [grid_, commands_] ->
            let
                numbers = commands_ 
                    |> ints
                    |> map (\n -> Move n)

                letters = commands_ 
                    |> randl
                    |> filterMap (\c ->
                            case c of
                                "R" -> Just TurnRight
                                "L" -> Just TurnLeft
                                _ -> Nothing
                        )

                commands = LE.interweave numbers letters

                rows = grid_
                    |> String.split "\n"
                    |> map (String.split "")

                toUnit a =
                    case a of
                        "." -> Floor
                        "#" -> Wall
                        _ -> Empty

                grid = List.indexedMap (\y line -> List.indexedMap (\x value -> ((x, y), toUnit value)) line) rows
                    |> List.concat
                    |> Dict.fromList
            in
            Just (grid, commands)
        _ -> Nothing

randl : String -> List String
randl input =
  let 
    regex = Maybe.withDefault Regex.never <|
      Regex.fromString "[R|L]+"
    found = Regex.find regex
  in
    List.map .match (found input)

toInitialState : Connections -> Connections -> (Grid, List Command) -> State
toInitialState connectionsExample connectionsPuzzle (grid, commands) =
    let
        blockSize = getBlockSize grid

        positions = 
            if blockSize == 4 then
                blockPositionsForExample blockSize
            else
                blockPositionsForPuzzle blockSize

        connections =
            if blockSize == 4 then
                connectionsExample
            else
                connectionsPuzzle
    in
    { grid = grid
    , commands = commands
    , blockSize = getBlockSize grid
    , positions = positions
    , connections = connections
    }

blockPositionsForPuzzle : Int -> Dict Int { left : Int, top : Int }
blockPositionsForPuzzle size =
    [ (1, { left = size, top = 0 } )
    , (2, { left = size * 2, top = 0 } )
    , (3, { left = size, top = size } )
    , (4, { left = 0, top = size * 2 } )
    , (5, { left = size, top = size * 2 } )
    , (6, { left = 0, top = size * 3 } )
    ]
        |> Dict.fromList

blockPositionsForExample : Int -> Dict Int { left : Int, top : Int }
blockPositionsForExample size =
    [ (1, { left = size * 2, top = 0 } )
    , (2, { left = 0, top = size } )
    , (3, { left = size, top = size } )
    , (4, { left = size * 2, top = size } )
    , (5, { left = size * 2, top = size * 2 } )
    , (6, { left = size * 3, top = size * 2 } )
    ]
        |> Dict.fromList

connectionsForExample1 : Connections
connectionsForExample1 =
    [ (1, [ (Left,1,Left), (Up,5,Up), (Right,1,Right), (Down,3,Down) ])
    , (2, [ (Left,4,Left), (Up,2,Up), (Right,3,Right), (Down,2,Down) ])
    , (3, [ (Left,2,Left), (Up,3,Up), (Right,4,Right), (Down,3,Down) ])
    , (4, [ (Left,3,Left), (Up,1,Up), (Right,2,Right), (Down,5,Down) ])
    , (5, [ (Left,3,Left), (Up,4,Up), (Right,6,Right), (Down,2,Down) ])
    , (6, [ (Left,5,Left), (Up,6,Up), (Right,5,Right), (Down,6,Down) ])
    ]
        |> Dict.fromList

connectionsForPuzzle1 : Connections
connectionsForPuzzle1 = 
    [ (1, [ (Left,2,Left), (Up,5,Up), (Right,2,Right), (Down,3,Down) ])
    , (2, [ (Left,1,Left), (Up,2,Up), (Right,1,Right), (Down,2,Down) ])
    , (3, [ (Left,3,Left), (Up,1,Up), (Right,3,Right), (Down,5,Down) ])
    , (4, [ (Left,5,Left), (Up,6,Up), (Right,5,Right), (Down,6,Down) ])
    , (5, [ (Left,4,Left), (Up,3,Up), (Right,4,Right), (Down,1,Down) ])
    , (6, [ (Left,6,Left), (Up,4,Up), (Right,6,Right), (Down,4,Down) ])
    ]
        |> Dict.fromList

solve : State -> Location
solve state =
    let
        getWrapPoint pos direction =
            let
                getCoordinate (x, y) block newDir =
                    Dict.get block state.positions
                        |> Maybe.map (\{left, top} ->
                                let
                                    xx = case direction of
                                        Up -> modBy state.blockSize x
                                        Right -> modBy state.blockSize y
                                        Down -> state.blockSize - 1 - (modBy state.blockSize x)
                                        Left -> state.blockSize - 1 - (modBy state.blockSize y)
                                in 
                                case newDir of
                                    Up -> (left + xx, top + state.blockSize - 1)
                                    Right -> (left, top + xx)
                                    Down -> (left + state.blockSize - 1 - xx, top)
                                    Left -> (left + state.blockSize - 1, top + state.blockSize - 1 - xx)
                            )

                getPoint (_, block, newDir) = 
                    Maybe.map2 Tuple.pair (getCoordinate pos block newDir) (Just newDir)

                currentBlock = getBlockNumber state.positions state.blockSize pos
                wrapPoint = Dict.get currentBlock state.connections
                    |> Maybe.andThen (LE.find (\(dir,_,_) -> dir == direction))
                    |> Maybe.andThen getPoint
            in
            case wrapPoint of
                Just location -> location
                _ -> (pos, direction)

        getNextPos (x, y) (dx, dy) dir =
            case Dict.get (x + dx, y + dy) state.grid of
                Just Wall -> ((x, y), dir)
                Just Floor -> ((x + dx, y + dy), dir)
                _ ->
                    let
                        (nextPoint, nextDir) = getWrapPoint (x, y) dir
                    in
                    case Dict.get nextPoint state.grid of
                        Just Wall -> ((x, y), dir)
                        _ -> (nextPoint, nextDir)

        oneStep (pos, direction) =
            case direction of
                Right -> getNextPos pos (1, 0) Right
                Left -> getNextPos pos (-1, 0) Left
                Up -> getNextPos pos (0, -1) Up
                Down -> getNextPos pos (0, 1) Down

        oneCommand command (pos, direction) =
            case command of
                Move n -> runNTimes n oneStep (pos, direction)
                TurnRight -> (pos, turnRight direction)
                TurnLeft -> (pos, turnLeft direction)
    in
    foldl oneCommand (getStartingPoint state.grid, Right) state.commands

turnRight : Direction -> Direction
turnRight direction =
    case direction of
        Up -> Right
        Right -> Down
        Down -> Left
        Left -> Up

turnLeft : Direction -> Direction
turnLeft direction =
    case direction of
        Up -> Left
        Right -> Up
        Down -> Right
        Left -> Down

getStartingPoint : Grid -> Coordinate
getStartingPoint grid = 
    grid
        |> Dict.toList
        |> LE.find (\((_,y), unit) ->
                case unit of
                    Empty -> False
                    _ -> y == 0
            )
        |> Maybe.withDefault ((0, 0), Floor)
        |> Tuple.first

getMaxX : Grid -> Int
getMaxX grid =
    Dict.keys grid 
        |> foldl (\(x,_) acc -> max acc x) 0

getMaxY : Grid -> Int
getMaxY grid =
    Dict.keys grid 
        |> foldl (\(_,y) acc -> max acc y) 0

getBlockNumber : Dict Int {left : Int, top : Int } -> Int -> Coordinate-> Int
getBlockNumber positions size (x, y) =
    positions
        |> Dict.toList
        |> LE.find (\(_, {left, top}) -> 
                (x >= left) && 
                (y >= top) && 
                (x < (left + size)) &&
                (y < (top + size))
            )
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 1

getBlockSize : Grid -> Int
getBlockSize grid = 
    let
        maxX = getMaxX grid
        maxY = getMaxY grid
    in
    if maxX > maxY then 
        (maxX + 1) // 4 
    else
        (maxX + 1) // 3

--------------------------------------------------------------

part2 : String -> Maybe Int
part2 input =
    input
        |> parseInput 
        |> Maybe.map (toInitialState connectionsExample2 connectionsPuzzle2)
        |> Maybe.map solve
        |> Maybe.map score

connectionsExample2 : Connections
connectionsExample2 =
     [ (1, [(Left,3,Down), (Up,2,Down), (Right,6,Left), (Down,4,Down)])
     , (2, [(Left,6,Up), (Up,1,Down), (Right,3,Right), (Down,5,Up)])
     , (3, [(Left,2,Left), (Up,1,Right), (Right,4,Right), (Down,5,Right)])
     , (4, [(Left,3,Left), (Up,1,Up), (Right,6,Down), (Down,5,Down)])
     , (5, [(Left,3,Up), (Up,4,Up), (Right,6,Right), (Down,2,Up)])
     , (6, [(Left,5,Left), (Up,4,Left), (Right,1,Left), (Down,2,Right)])
     ]
         |> Dict.fromList

connectionsPuzzle2 : Connections
connectionsPuzzle2 = 
    [ (1, [(Left,4,Right), (Up,6,Right), (Right,2,Right), (Down,3,Down)])
     , (2, [(Left,1,Left), (Up,6,Up), (Right,5,Left), (Down,3,Left)])
     , (3, [(Left,4,Down), (Up,1,Up), (Right,2,Up), (Down,5,Down)])
     , (4, [(Left,1,Right), (Up,3,Right), (Right,5,Right), (Down,6,Down)])
     , (5, [(Left,4,Left), (Up,3,Up), (Right,2,Left), (Down,6,Left)])
     , (6, [(Left,1,Down), (Up,4,Up), (Right,5,Up), (Down,2,Down)])
     ]
         |> Dict.fromList

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
