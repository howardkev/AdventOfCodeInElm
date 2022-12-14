module Day13 exposing (..)

import View exposing (..)
import Util exposing (..)
import Browser
import List.Extra as LE
import Regex
import Dict exposing (Dict)
import Set exposing (Set)
import List exposing (map, filterMap, foldl, take, filter, concat, length, range, sort, sum, maximum, sortWith, head, tail, any, all, reverse)
import String exposing (lines, split, toList)
import Dict exposing (empty)
import Fifo exposing (Fifo)
import GridExample exposing (Board)

todayDescription : PuzzleDescription
todayDescription = { day = 13, title = "Distress Signal" }

sampleInput : String
sampleInput =
    """
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
"""

type Tree a
    = Empty
    | Node (List (Tree a))
    | Leaf a

type alias Packet = Tree Int
type alias PacketPair = { left : Packet, right : Packet }

part1 : String -> Int
part1 input =
    input
        |> parseInput
        |> map comparePackets
        |> score

parseInput : String -> List PacketPair
parseInput input =
    input
        |> String.split "\n\n"
        |> filterMap parseGroup

parseGroup : String -> Maybe PacketPair
parseGroup lines =
    lines
        |> String.lines
        |> map (String.split "")
        |> toLeftRight

toLeftRight : List (List String) -> Maybe PacketPair
toLeftRight lists =
    case lists of
        a :: b :: [] -> 
            Just { left = combineDigits a |> toTree
                 , right = combineDigits b |> toTree }
        _ -> Nothing
    
combineDigits : List String -> List String
combineDigits list =
    let
        iter c (acc, num) =
            case String.toInt c of
                Nothing -> 
                    if String.length num > 0 then
                        (acc ++ [num, c], "")
                    else
                    (acc ++ [c], num)
                _ -> 
                    (acc, num ++ c)
    in
    Tuple.first <| List.foldl iter ([], "") list

toTree : List String -> Packet
toTree chars =
    (List.foldl (\char stack -> 
        case char of
            "," -> stack
            "[" -> 
                (Node []) :: stack
            "]" ->
                case stack of
                    [] -> stack
                    top :: remaining ->
                        case remaining of
                            [] -> stack
                            parent :: r2 ->
                                (addChild top parent) :: r2
                            
            a -> 
                case stack of
                    [] -> stack
                    top :: remaining ->
                        let
                            newLeaf = Leaf (String.toInt a |> Maybe.withDefault 0)
                            t = addChild newLeaf top
                        in
                        t :: remaining
    ) [Empty] chars)
        |> head |> Maybe.withDefault Empty

addChild : Tree a -> Tree a -> Tree a
addChild child tree =
    case tree of
        Empty -> child
        Node children -> Node (children ++ [child])
        Leaf _ -> tree

comparePackets : PacketPair -> Order
comparePackets { left, right } =
    compareTrees left right

compareTrees : Packet -> Packet -> Order
compareTrees tree1 tree2 =
    case (tree1, tree2) of
        (Leaf a, Leaf b) -> 
            compare a b 
        (Leaf _, Node _) ->
            compareTrees (Node [tree1]) tree2
        (Node _, Leaf _) ->
            compareTrees tree1 (Node [tree2])
        (Node children1, Node children2) ->
            let
                nodeCompare index acc =
                    if acc == EQ then
                        let
                            child1 = LE.getAt index children1
                            child2 = LE.getAt index children2
                            result = case (child1, child2) of
                                (Just a, Just b) ->
                                    compareTrees a b
                                (Just _, Nothing) ->
                                    GT
                                (Nothing, Just _) ->
                                    LT
                                (Nothing, Nothing) ->
                                    acc
                        in
                        result
                    else
                    acc
                    
                len1 = length children1
                len2 = length children2
                toCheck = range 0 ((min len1 len2) + 1)
            in
            foldl nodeCompare EQ toCheck
        _ -> EQ

score : List Order -> Int
score compares =
    let
        result = foldl (\comp (index, sum) ->
            if comp == LT then
                (index + 1, sum + index)
            else
                (index + 1, sum)
            ) (1, 0) compares
    in
    Tuple.second result

printPacket : Packet -> String
printPacket packet =
    case packet of
        Empty -> "Empty"
        Node children -> 
            "[" ++ (foldl (\child acc -> acc ++ printPacket child) "" children) ++ "]"
        Leaf n -> 
            (String.fromInt n) ++ " "

--------------------------------------------------------------

part2 : String -> Int
part2 input =
   input
        |> parseInput
        |> List.concatMap toArrays
        |> addMarkers
        |> List.sortWith (\a b -> compareTrees a b)
        |> score2
        
toArrays : PacketPair -> List Packet
toArrays packet =
    [packet.left, packet.right]
    
marker1 : Packet
marker1 = Node [Node [Leaf 2]]

marker2 : Packet
marker2 = Node [Node [Leaf 6]]

addMarkers : List (Packet) -> List (Packet)
addMarkers list =
    marker1 :: marker2 :: list
    
score2 : List (Packet) -> Int
score2 sortedPackets =
    let
        marker1Index = LE.findIndex (\n -> compareTrees n marker1 == EQ) sortedPackets |> Maybe.withDefault 0
        marker2Index = LE.findIndex (\n -> compareTrees n marker2 == EQ) sortedPackets |> Maybe.withDefault 0
    in
    (marker1Index + 1) * (marker2Index + 1)

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
