module Tests

open System
open Xunit
open FsUnit
open Day3.Program


let example = [
    "467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."
]

let getParsed () =
    example
    |> addCoordinatesToCharacters
    |> List.map bundle

[<Fact>]
let ``Parse numbers in line`` () =
    let result = ["467..114.."] |> addCoordinatesToCharacters
    result[0] |> should equal [
       ((0,0), Digit "4")
       ((1,0), Digit "6")
       ((2,0), Digit "7")
       ((3,0), Character.Empty)
       ((4,0), Character.Empty)
       ((5,0), Digit "1")
       ((6,0), Digit "1")
       ((7,0), Digit "4")
       ((8,0), Character.Empty)
       ((9,0), Character.Empty)
       ]


[<Fact>]
let ``bundle line`` () =

    let result = 
        ["467..114.."]
        |> addCoordinatesToCharacters
        |> List.map bundle

    result[0]
    |> should equal  [
        ([(0, 0); (1, 0); (2, 0)], Number 467);
        ([(3, 0)], Empty);
        ([(4, 0)], Empty);
        ([(5, 0); (6, 0); (7, 0)], Number 114);
        ([(8, 0)], Empty);
        ([(9, 0)], Empty)]
    
    

    
