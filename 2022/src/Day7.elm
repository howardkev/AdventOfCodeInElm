module Day7 exposing (..)

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
todayDescription = { day = 7, title = "No Space Left On Device" }

sampleInput : String
sampleInput =
    String.trim """
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"""

type alias Terminal =
    { lines : List TerminalLine
    , sizes : Sizes
    , currentDir : List String}
        
type TerminalLine =
    ListDir
    | File String Int
    | Directory String
    | ChangeDir ChangeType

type alias Sizes = Dict (List String) Int
 
type ChangeType =
    Root
    | ToDir String
    | Parent

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> .sizes
        |> Dict.values
        |> List.filter (\x -> x <= 100000)
        |> List.sum

parseInput : String -> Terminal
parseInput input =
    input
        |> split "\n"
        |> List.filterMap toTerminalLines
        |> toTerminal
        |> execute

toTerminalLines : String -> Maybe TerminalLine
toTerminalLines string =
    case String.words string of
        ["$", "ls"] -> Just ListDir
        ["$", "cd", "/"] -> Just (ChangeDir Root)
        ["$", "cd", ".."] -> Just (ChangeDir Parent)
        ["$", "cd", dirName] -> Just (ChangeDir (ToDir dirName))
        ["dir", dirName] -> Just (Directory dirName)
        [fileSize, fileName] 
            -> Maybe.map2 File (Just fileName) (String.toInt fileSize)
        _ -> Nothing

toTerminal : List TerminalLine -> Terminal
toTerminal terminalLines =
    { lines = terminalLines
    , sizes = Dict.empty
    , currentDir = [] }

execute : Terminal -> Terminal
execute terminal =
    case terminal.lines of
        [] -> terminal
        x::xs ->
            let
                nextTerminal = processOneLine x terminal
            in
            execute { nextTerminal | lines = xs }
            
processOneLine : TerminalLine -> Terminal -> Terminal
processOneLine line terminal =
    case line of
        ChangeDir Root -> 
            { terminal | currentDir = ["/"] }
        ChangeDir (ToDir name) -> 
            { terminal | currentDir = terminal.currentDir ++ [name]}
        ChangeDir Parent -> 
            { terminal | currentDir = terminal.currentDir 
                |> List.reverse |> List.drop 1 |> List.reverse }
        File _ size ->
            { terminal | sizes = updateSizes size terminal }
        _ -> terminal
     
updateSizes : Int -> Terminal -> Sizes
updateSizes size terminal =
    let
        updateDict dir sizes = Dict.update dir (updateMaybeSize size) sizes
        dirs = LE.inits terminal.currentDir |> List.drop 1
    in
    List.foldl updateDict terminal.sizes dirs
        
updateMaybeSize : Int -> Maybe Int -> Maybe Int
updateMaybeSize size current =
    case current of
        Nothing -> Just size
        Just x -> Just (x + size)

------------------------------------------------------

part2 : String -> Maybe Int
part2 input =
    input
        |> parseInput
        |> solve2
        |> Dict.values
        |> List.minimum

solve2 : Terminal -> Sizes
solve2 terminal =
    let
        total = Dict.get ["/"] terminal.sizes
            |> Maybe.withDefault 0
        space = 70000000 - total
        need = 30000000 - space
    in
    Dict.filter (\_ v -> v >= need) terminal.sizes

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
