// For more information see https://aka.ms/fsharp-console-apps
open System

let input = [|
    "1abc2"
    "pqr3stu8vwx"
    "a1b2c3d4e5f"
    "treb7uchet"|]


let convertToNumbers  (p:string array) =
        p 
        |> Seq.map (fun s -> 
            let isDigit c = c |> Char.IsDigit

            let firstDigit = s |> Seq.find isDigit |> string |> int
            let lastDigit = s |> Seq.findBack isDigit |> string |> int

            firstDigit * 10 + lastDigit)
            
        |> Seq.sum

convertToNumbers input |> printfn "%i"

// Part 2

let input2 = System.IO.File.ReadAllLines "input.txt"

let digitsAsStrings = 
    [|
        ("one" , "1")
        ("two", "2") 
        ("three", "3")
        ("four", "4")
        ("five","5")
        ("six", "6")
        ("seven", "7")
        ("eight", "8")
        ("nine", "9")
        ("1" , "1")
        ("2", "2") 
        ("3", "3")
        ("4", "4")
        ("5","5")
        ("6", "6")
        ("7", "7")
        ("8", "8")
        ("9", "9")
    |]

let generateSum (lines: string array) =
    let integers = 
        lines 
        |> Seq.map (fun line -> 
            let first = 
                digitsAsStrings 
                |> Array.map (fun (f, s)  -> (line.IndexOf f, s)) 
                |> Array.filter (fun (i, n) -> i <> -1)
                |> Array.sortBy (fun (i, n) -> i)
                |> Array.head
                |> snd 
                |> int

            let second = 
                digitsAsStrings 
                |> Array.map (fun (f, s)  -> (line.LastIndexOf f, s)) 
                |> Array.filter (fun (i, n) -> i <> -1)
                |> Array.sortBy (fun (i, n) -> i) 
                |> Array.last
                |> snd 
                |> int

            10 * first + second )
    integers |> Seq.sum

input |> generateSum |> printfn "%i"
input2 |> generateSum |> printfn "%i"

let input3 = [|
    "nkzjrdqrmpztpqninetwofour1znnkd"
    "s5sevenxrdfr4mhpstgbjcfqckronesix"
    "3four4"
    "sfdrtpvspsixsn5zbqmggb8vgkjseight"
|]

input3 
|> generateSum
|> printfn "%i"


