module Tests.Day2

open System
open Xunit
open Day2.Types
open Day2.Part1
open Day2.Common
open FsUnit
open System.IO

module Part1 =
    [<Fact>]
    let ``Parse reveal`` () =
        let reveal = "3 blue, 4 red";
        let result = parseReveal reveal

        result |> should contain { Color = Color.Red; Count = 4}
        result |> should contain { Color = Color.Blue; Count = 3}

    [<Fact>]
    let ``Parse reveal 2`` () =
        let reveal = "1 red, 2 green";
        let result = parseReveal reveal

        result |> should contain { Color = Color.Red; Count = 1}
        result |> should contain { Color = Color.Green; Count = 2}

    [<Fact>]
    let `` Parse sets `` () =
        let sets = "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
        let result = parseSets sets 
        result.Length |> should equal 3
        result |> should contain [ {Color = Color.Blue; Count= 3}; {Color = Color.Red; Count=4} ]
        result |> should contain [{Color = Color.Red; Count=1}; {Color = Color.Green; Count= 2};{Color= Color.Blue; Count = 6}]
        result |> should contain [{Color = Color.Green; Count=2}]

    [<Fact>]
    let `` Sum up sets `` () =
        let sets = [ 
            [{Color = Color.Blue; Count= 3}; {Color = Color.Red; Count=4} ]
            [{Color = Color.Red; Count=1}; {Color = Color.Green; Count= 2};{Color= Color.Blue; Count = 6}]
            [{Color = Color.Green; Count=2}]
            ]

        let result = sumUpSets sets 
        result |> should contain {Color = Color.Blue; Count= 9}
        result |> should contain {Color = Color.Red; Count=5}
        result |> should contain {Color = Color.Green; Count=4}

    [<Fact>]
    let `` Parse game id `` () =
        let gameLine = "Game 1"
        let result = parseGameId gameLine
        result |> should equal 1

    [<Fact>]
    let `` Parse game line`` () =
        let gameLine = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
        let result = parseGame gameLine
        result.Id |> should equal 1
        result.CubeSets |> should contain [{ Color = Color.Blue; Count= 3}; {Color = Color.Red; Count=4}]
        result.CubeSets |> should contain [{Color = Color.Red; Count=1}; { Color = Color.Green; Count = 2}; { Color = Color.Blue; Count= 6}]
        result.CubeSets |> should contain [{ Color = Color.Green; Count= 2}]



    [<Fact>]
    let `` Parse game lines`` () =
        let gameLines = [
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
        ]

        let result = parseGames gameLines
        result.Length |> should equal 5

        let expected : Id list =  [1..5]
        result |> List.map (fun x -> x.Id) |> should equal expected


    [<Fact>]
    let `` Get valid game ids`` () =
        let gameLines = [
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
        ]

        let result = parseGames gameLines |> getValidGames [
            { Color = Color.Red ; Count = 12}
            { Color = Color.Green ; Count = 13}
            { Color = Color.Blue ; Count = 14}
        ]

        result.Length |> should equal 3

        let expected : Id list =  [1;2;5]
        result |> List.map (fun x -> x.Id) |> should equal expected
        result |> List.map (fun x -> x.Id) |> List.sum |> should equal 8

    [<Fact>]
    let `` Get valid game ids 2`` () =
        let gameLines = [
            "Game 5: 14 green, 3 red, 3 blue; 2 red, 1 green, 1 blue; 8 green, 3 blue, 1 red; 15 green, 8 blue, 1 red"
            "Game 6: 4 blue, 8 green, 5 red; 9 green, 10 blue, 7 red; 11 blue, 10 red, 7 green; 8 red, 6 blue, 9 green"
            "Game 7: 5 green, 11 blue, 9 red; 2 green, 6 red, 12 blue; 8 red, 4 blue, 3 green; 7 green, 8 red, 9 blue; 8 green, 5 red"
            "Game 8: 7 red, 12 green; 9 blue, 15 red, 8 green; 3 blue, 11 green, 6 red; 8 blue, 12 red, 5 green"
            "Game 9: 8 blue, 6 red, 7 green; 2 blue, 3 red, 10 green; 10 blue, 6 red, 7 green; 11 red, 7 blue, 5 green; 10 red, 11 green"
        ]

        let parsedGames  = parseGames gameLines 
        let result = parsedGames |> getValidGames [
            { Color = Color.Red ; Count = 12}
            { Color = Color.Green ; Count = 13}
            { Color = Color.Blue ; Count = 14}
        ]

        result.Length |> should equal 3

        let expected : Id list =  [6;7;9]
        result |> List.map (fun x -> x.Id) |> should equal expected
        result |> List.map (fun x -> x.Id) |> List.sum |> should equal 22 

    [<Fact>]
    let `` Get valid game ids with puzzle input`` () =

        let gameLines = File.ReadAllLines "Day2Part12PuzzleInput.txt" |> List.ofArray

        let result = parseGames gameLines |> getValidGames [
            { Color = Color.Red ; Count = 12}
            { Color = Color.Green ; Count = 13}
            { Color = Color.Blue ; Count = 14}
        ]

        result |> List.map (fun x -> x.Id) |> List.sum |> should equal 2101

module Part2 = 
    open Day2.Part2

    [<Fact>]
    let `` Find minimum amout of cubes per game`` () =
        let gameLines = [
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
            ]

        let parsedGames = parseGames gameLines
        let g = parsedGames |> List.head
        let result = getMinNumberOfCubes g
        result |> should contain {Color = Color.Red; Count = 4}
        result |> should contain {Color = Color.Blue; Count = 6}
        result |> should contain {Color = Color.Green; Count = 2}

    [<Fact>]
    let `` calculate power of all cubes`` () =
        let gameLines = [
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
            ]

        let games = parseGames gameLines
        let result = calcPowerOfAllGames games
        result |> should equal 2286

    [<Fact>]
    let `` calculate power of all cubes, puzzle input`` () =

        File.ReadAllLines "Day2Part12PuzzleInput.txt"
        |> List.ofArray
        |> parseGames
        |> calcPowerOfAllGames
        |> should equal 58269
