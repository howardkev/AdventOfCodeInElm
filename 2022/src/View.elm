module View exposing (..)

import Element exposing (Element, Color, Attribute, rgb255, text, scrollbars, paragraph, centerX, centerY, padding, row, width, height, el, fill, spacing, alignLeft, alignRight, layout, column, fillPortion) 
import Element.Border as Border 
import Element.Background as Background
import Element.Region as Region
import Element.Input as Input
import Element.Font as Font
import Http
import Html exposing (Html)

type alias PuzzleDescription = 
    { day: Int, title: String }

type Msg
    = Puzzle1 
    | Puzzle2 
    | LoadInput 
    | UsePuzzle 
    | UseExample 
    | GotInput (Result Http.Error String)

type alias Model =
    { input : String
    , puzzleInput : Maybe String
    , result : Maybe String
    , description : PuzzleDescription }

color : { blue : Color, darkCharcoal : Color, lightBlue : Color, lightGrey : Color, white : Color }
color =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , white = rgb255 0xFF 0xFF 0xFF
    }

view : Model -> Html Msg
view model =
    layout [] <|
        column [ width fill, height fill, spacing 5, padding 5] <|
            [ titleBar model.description
            , inputDisplay model.input
            , solveButtons model.puzzleInput
            , resultDisplay model.result
            ]

titleBar : PuzzleDescription -> Element msg
titleBar description =
    el [ Region.heading 1, width fill, padding 10, centerX, Background.color color.lightGrey] <|
        text ("--- Day " ++ String.fromInt description.day ++ ": " ++ description.title ++ " ---")

inputDisplay : String -> Element msg
inputDisplay input =
    el [ Font.family [Font.monospace], scrollbars, padding 10, Border.width 2, Border.rounded 6, width fill, height (fillPortion 1)] <|
        column [] <|
            List.map oneLine (String.split("\n") input)

resultDisplay : Maybe String -> Element msg
resultDisplay result =
    el [Font.family [Font.monospace], Border.width 2, Border.rounded 6, width fill, height fill, scrollbars ] <|
        --paragraph [ centerX, centerY ] [toResult result]
        column [] <|
            List.map oneLine (String.split "\\n" (Maybe.withDefault "Click which part to solve" result))

oneLine : String -> Element msg
oneLine line =
    if line == "" then
        el [] <| text "---------"
    else
        el [] <| text line

toResult : Maybe String -> Element msg
toResult str =
    case str of
        Just value -> text value
        Nothing -> text "Click which part to solve"

button : Attribute msg -> msg -> String -> Element msg
button align msg label =
    Input.button [align, padding 5, Border.width 3, Border.rounded 6, Border.color color.blue, Background.color color.lightBlue] 
        { onPress = Just msg, label = text label }

getReturnedText result =
    case result of
        Ok fulltext -> Just fulltext
        _ -> Nothing 

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

solveButtons : Maybe String -> Element Msg
solveButtons loaded =
    row [ width fill, spacing 5]
        ([ button alignLeft Puzzle1 "Part 1"
        , button alignLeft Puzzle2 "Part 2"
        ] ++
        case loaded of
            Just _ -> 
                [ button alignRight UseExample "Example"
                , button alignRight UsePuzzle "Puzzle" ]
            Nothing ->
                [ button alignRight LoadInput "Load" ])

mainUpdate : Msg -> Model -> String -> Maybe String -> Maybe String -> ( Model, Cmd Msg )
mainUpdate msg model sample p1 p2 =
    case msg of
        Puzzle1 -> (
            { model | result = p1 }
            , Cmd.none)
        Puzzle2 -> (
            { model | result = p2 }
            , Cmd.none)
        LoadInput ->
            (model, 
                Http.get
                    { url = "http://localhost:3000/input/input" ++ (String.fromInt model.description.day) ++ ".txt"
                      , expect = Http.expectString GotInput
                    })
        GotInput result ->
            ({ model | puzzleInput = getReturnedText result }, Cmd.none)
        UsePuzzle ->
            ({ model | input = Maybe.withDefault "Error" model.puzzleInput }, Cmd.none)
        UseExample ->
            ({ model | input = String.trim sample }, Cmd.none)
