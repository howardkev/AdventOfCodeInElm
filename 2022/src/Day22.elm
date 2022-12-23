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

type alias State = 
    { grid: Grid
    , commands: List Command
    }

part1 : String -> Maybe Int
part1 input =
    input
        |> parseInput
        |> Maybe.map solve
        |> Maybe.map score

score : Location -> Int
score ((x,y), direction) =
    1000 * (y + 1) + 4 * (x + 1) + case direction of
        Right -> 0
        Down -> 1
        Left -> 2
        Up -> 3

parseInput : String -> Maybe State
parseInput input =
    input
        |> String.split "\n\n"
        |> parseBoth

parseBoth : List String -> Maybe State
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
            Just { grid = grid, commands = commands }
        _ -> Nothing

randl : String -> List String
randl input =
  let 
    regex = Maybe.withDefault Regex.never <|
      Regex.fromString "[R|L]+"
    found = Regex.find regex
  in
    List.map .match (found input)

blockPositionsForPuzzle size =
    [ (1, { left = size, top = 0 } )
    , (2, { left = size * 2, top = 0 } )
    , (3, { left = size, top = size } )
    , (4, { left = 0, top = size * 2 } )
    , (5, { left = size, top = size * 2 } )
    , (6, { left = 0, top = size * 3 } )
    ]
        |> Dict.fromList

blockPositionsForExample size =
    [ (1, { left = size * 2, top = 0 } )
    , (2, { left = 0, top = size } )
    , (3, { left = size, top = size } )
    , (4, { left = size * 2, top = size } )
    , (5, { left = size * 2, top = size * 2 } )
    , (6, { left = size * 2, top = size * 2 } )
    ]
        |> Dict.fromList

blockConnectionsForExample1 =
    [ (1, [ (Left,1,Left), (Up,5,Up), (Right,1,Right), (Down,3,Down) ])
    , (2, [ (Left,4,Left), (Up,2,Up), (Right,3,Right), (Down,2,Down) ])
    , (3, [ (Left,2,Left), (Up,3,Up), (Right,4,Right), (Down,3,Down) ])
    , (4, [ (Left,3,Left), (Up,1,Up), (Right,2,Right), (Down,5,Down) ])
    , (5, [ (Left,3,Left), (Up,4,Up), (Right,6,Right), (Down,2,Down) ])
    , (6, [ (Left,5,Left), (Up,6,Up), (Right,5,Right), (Down,6,Down) ])
    ]
        |> Dict.fromList

blockConnectionsForPuzzle1 = blockConnectionsForExample1

solve : State -> Location
solve state =
    let
        blockSize = getBlockSize state.grid

        blockPositions = 
            if blockSize == 4 then
                blockPositionsForExample blockSize
            else
                blockPositionsForPuzzle blockSize

        blockConnections = 
            if blockSize == 4 then
                blockConnectionsForExample1
            else
                blockConnectionsForPuzzle1

        getWrapPoint pos direction =
            let
                getCoordinate (x, y) block newDir =
                    Dict.get block blockPositions
                        |> Maybe.map (\{left, top} ->
                                case newDir of
                                    Right -> (left, y)
                                    Left -> (left + blockSize - 1, y)
                                    Down -> (x, top)
                                    Up -> (x, top + blockSize - 1)
                            )

                getPoint (_, block, newDir) = 
                    Maybe.map2 Tuple.pair (getCoordinate pos block newDir) (Just newDir)

                currentBlock = getBlockNumber blockPositions blockSize pos
                wrapPoint = Dict.get currentBlock blockConnections
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

--part2 : String -> Maybe Int
part2 input =
    input
        |> parseInput
        --|> Maybe.map solve2
        --|> Maybe.map score

-- blockConnectionsForExample2 =
--     [ (1, [(West, Top, 3), (North, Top, 2), (East, Right, 6), (South, Top, 4)])
--     , (2, [(West, Bottom, 6), (North, Top, 1), (East, Left, 3), (South, Bottom, 5)])
--     , (3, [(West, Right, 2), (North, Left, 1), (East, Left, 4), (South, Left, 5)])
--     , (4, [(West, Right, 3), (North, Bottom, 1), (East, Top, 6), (South, Top, 5)])
--     , (5, [(West, Bottom, 3), (North, Bottom, 4), (East, Left, 6), (South, Bottom, 2)])
--     , (6, [(West, Right, 5), (North, Right, 4), (East, Right, 1), (South, Left, 2)])
--     ]
--         |> Dict.fromList

-- blockConnectionsForPuzzle2 =
--     [ (1, [(West, Top, 3), (North, Top, 2), (East, Right, 6), (South, Top, 4)])
--     , (2, [(West, Bottom, 6), (North, Top, 1), (East, Left, 3), (South, Bottom, 5)])
--     , (3, [(West, Right, 2), (North, Left, 1), (East, Left, 4), (South, Left, 5)])
--     , (4, [(West, Right, 3), (North, Bottom, 1), (East, Top, 6), (South, Top, 5)])
--     , (5, [(West, Bottom, 3), (North, Bottom, 4), (East, Left, 6), (South, Bottom, 2)])
--     , (6, [(West, Right, 5), (North, Right, 4), (East, Right, 1), (South, Left, 2)])
--     ]
--         |> Dict.fromList

-- solve2 : State -> Location
-- solve2 state =
--     let
--         blockSize = getBlockSize state.grid
--         blockPositions = 
--             if blockSize == 4 then
--                 blockPositionsForExample blockSize
--             else
--                 blockPositionsForPuzzle blockSize

--         blockConnections = 
--             if blockSize == 4 then
--                 blockConnectionsForExample2
--             else
--                 blockConnectionsForPuzzle2

--         getWithWrap dir (x, y) (dx, dy) wrapPoint =
--             case Dict.get (x + dx, y + dy) state.grid of
--                 Just Wall -> ((x + dx, y + dy), dir)
--                 Just Floor -> ((x + dx, y + dy), dir)
--                 _ -> wrapPoint

--         getNextPos dir (x, y) (dx, dy) wrapPoint =
--             let
--                 (next, nextDir) = getWithWrap dir (x, y) (dx, dy) wrapPoint
--             in
--             case Dict.get next state.grid of
--                 Just Wall -> ((x, y), dir)
--                 Just Floor -> (next, nextDir)
--                 _ -> ((x, y), dir) --Debug.todo "bad wrap"

--         getRightWrapPos (x, y) =
--             let
--                 block = getBlockNumber blockPositions blockSize (x,y)
--             in
--             if block == 2 || block == 5 then
--                 ((x,y), West)
--             else if block == 1 then
--                 ((0, 149 - y), East)
--             else if block == 3 then
--                 ((y - 50, 100), South)
--             else if block == 4 then
--                 ((50, 49 - (y - 100)), East)
--             else -- block 6
--                 ((50 + (y - 150), 0), South)

--         getLeftWrapPos (x, y) =
--             let
--                 block = getBlockNumber blockPositions blockSize (x,y)
--             in
--             if block == 1 || block == 4 then
--                 ((x,y), East)
--             else if block == 2 then
--                 ((99, 149 - y), West)
--             else if block == 3 then
--                 ((100 + (y - 50), 49), North)
--             else if block == 5 then
--                 ((149, 49 - (y - 100)), West)
--             else -- block 6
--                 ((50 + (y - 150), 149), North)

--         getTopWrapPos (x, y) =
--             let
--                 block = getBlockNumber blockPositions blockSize (x,y)
--             in
--             if block == 1 || block == 3 || block == 4 then
--                 ((x,y), South)
--             else if block == 2 then
--                 ((99, 50 + (x - 100)), West)
--             else if block == 5 then
--                 ((49, 150 + (x - 50)), West)
--             else -- block 6
--                 ((100 + x, 0), South)

--         getBottomWrapPos (x, y) =
--             let
--                 block = getBlockNumber blockPositions blockSize (x,y)
--             in
--             if block == 3 || block == 5 || block == 6 then
--                 ((x,y), North)
--             else if block == 1 then
--                 ((0, 150 + (x - 50)), East)
--             else if block == 2 then
--                 (((x - 100), 199), North)
--             else -- block 4
--                 ((50, 50 + x), East)

--         move (pos, direction) =
--             case direction of
--                 East -> getNextPos direction pos (1, 0) (getLeftWrapPos pos)
--                 West -> getNextPos direction pos (-1, 0) (getRightWrapPos pos)
--                 North -> getNextPos direction pos (0, -1) (getBottomWrapPos pos)
--                 South -> getNextPos direction pos (0, 1) (getTopWrapPos pos)

--         oneStep command (pos, direction) =
--             case command of
--                 Move n -> runNTimes n move (pos, direction)
--                 TurnRight -> (pos, turnRight direction)
--                 TurnLeft -> (pos, turnLeft direction)
--     in
--     foldl oneStep (getStartingPoint state.grid, East) state.commands

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
