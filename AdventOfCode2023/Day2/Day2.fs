module AdventOfCode2023.Day2.Day2

open System
open System.IO
open Xunit
open Xunit.Abstractions
open AdventOfCode2023.Helpers

// common types and functions

type Bag = Map<string, int>
type HandValue = string * int
type Hand = seq<HandValue>
type Hands = seq<Hand>
type GameId = int
type Game = { Id: GameId; Hands: Hands }

let parseHandValue (unparsedHandValue: string) : HandValue =
    match unparsedHandValue.Split(" ") |> TrimAll with
    | [| amount; color |] -> color, int amount
    | _ -> failwith $"couldn't parse handValue {unparsedHandValue}"

let parseHand (unparsedHand: string) : Hand =
    unparsedHand.Split(",") |> TrimAll |> Seq.map (parseHandValue)

let parseHands (gameState: string) : Hands =
    let unparsedHands = gameState.Split(";") |> TrimAll
    unparsedHands |> Seq.map (parseHand)

let parseGame (line: string) : Game =
    let rawGameId, gameState =
        match line.Split(":") |> TrimAll with
        | [| gameId; gameState |] -> gameId, gameState
        | _ -> failwith $"couldn't get gameId and gameState from line {line}"

    let gameId =
        match rawGameId.Split(" ") |> TrimAll with
        | [| _; gameValue |] -> int gameValue
        | _ -> failwith $"couldn't parse gameValue from gameId {rawGameId}"

    let hands = parseHands gameState
    { Hands = hands; Id = gameId }

// functions for part 1

let bagAllowsHandValue (bag: Bag) (handValue: HandValue) : bool =
    let color, value = handValue
    let bagValue = bag |> Map.tryFind color

    match bagValue with
    | Some bagValue -> bagValue >= value
    | None -> false

let bagAllowsHand (bag: Bag) (hand: Hand) : bool =
    hand |> Seq.map (bagAllowsHandValue bag) |> Seq.forall id

let bagAllowsGame (bag: Bag) (game: Hands) : bool =
    game |> Seq.map (bagAllowsHand bag) |> Seq.forall id


let getValueOfLine (bag: Bag) (line: string) =
    if (String.IsNullOrWhiteSpace line) then
        0
    else
        let game = parseGame line

        match bagAllowsGame bag game.Hands with
        | true -> game.Id
        | false -> 0

let getSumOfPossibleGameIds (input: string) (bag: Bag) =
    let lines = input.Split("\n") |> TrimAll
    let lineValues = lines |> Seq.map (getValueOfLine bag)
    lineValues |> Seq.reduce (+)

// functions for part 2

let getMinimumBagAllowingGame (game: Game) : Bag =
    game.Hands
    |> Seq.collect id // flatten to HandValues
    |> Seq.groupBy fst // group by colors
    |> Seq.map (fun (color, handValues) -> Seq.maxBy snd handValues) // map groups to the max value
    |> Map.ofSeq


let getPowerValueOfBag (bag: Bag) : int =
    bag |> Map.toSeq |> Seq.map snd |> Seq.reduce (*)


let getPowerSumOfMinimalBags (input: string) : int =
    let rawGames = input.Split("\n") |> TrimAll |> Seq.filter isNonWhiteSpace
    let games = rawGames |> Seq.map (parseGame)

    games
    |> Seq.map (getMinimumBagAllowingGame)
    |> Seq.map getPowerValueOfBag
    |> Seq.reduce (+)


type MyTests(output: ITestOutputHelper) =
    let DefaultBag =
        Bag[("red", 12)
            ("green", 13)
            ("blue", 14)]

    let ExampleInput =
        """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""

    [<Fact>]
    let TestGetSumOfPossibleGameIds () =
        let result = getSumOfPossibleGameIds ExampleInput DefaultBag
        Assert.Equal(8, result)

    [<Fact>]
    let GetOutputPartOne () =
        let input = File.ReadAllText("Day2/input.txt")
        output.WriteLine($"{getSumOfPossibleGameIds input DefaultBag}")


    [<Fact>]
    let TestGetPowerSumOfMinimalBag () =
        let result = getPowerSumOfMinimalBags ExampleInput
        Assert.Equal(2286, result)

    [<Fact>]
    let GetOutputPartTwo () =
        let input = File.ReadAllText("Day2/input.txt")
        output.WriteLine($"{getPowerSumOfMinimalBags input}")
