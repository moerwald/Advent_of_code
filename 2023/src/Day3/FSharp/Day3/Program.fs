
module Day3.Program

type Coordinates = int * int

type Character =
    | Empty
    | Digit of string
    | Symbol of char

type Bundle =
    | Empty 
    | Number of int
    | Symbol of string

let addCoordinatesToCharacters (lines : string list) =
    [for (y, line) in lines |> Seq.indexed do 
        [for (x, c) in line |> Seq.indexed ->  
            match c with
            | c when System.Char.IsDigit(c) -> Coordinates (x,y), c |> string |> Digit
            | '.' -> Coordinates (x,y), Character.Empty 
            | _ -> Coordinates (x,y),  Character.Symbol c
        ]
     ]

let rec bundle (line: (Coordinates * Character ) list) =
    match line with
    | [] -> []
    | (_ , Digit _ )::t ->
        let adjacentDigits = line |> List.takeWhile (fun (c, e) -> 
            match e with
            | Digit _ -> true
            | _ -> false)

        let nr = 
            adjacentDigits
            |> List.map ( fun (_, ch) -> match ch with | Digit d -> d | _ -> "")  
            |> String.concat ""
            |> int
            |> Number

        let coordinates = adjacentDigits |> List.map fst

        (coordinates, nr):: bundle(line |> List.skip (adjacentDigits.Length))
    | (co, Character.Empty)::t -> ([co], Bundle.Empty)::bundle(line |> List.skip 1)
    | (co, Character.Symbol s)::t -> ([co], Bundle.Symbol (s|> string))::bundle(line |> List.skip 1)

let parsed = 
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\\input_part1.txt"
    |> List.ofArray
    |> addCoordinatesToCharacters 
    |> List.map bundle

let getSymbolCoordinates () = 
    parsed
    |> List.collect id
    |> List.filter(fun (_ , b) -> match b with | Bundle.Symbol _ -> true | _ -> false)
    |> List.map fst
    |> List.collect id

let getNumbers () = 
    parsed
    |> List.collect id
    |> List.filter (fun (_, b) -> match b with | Bundle.Number _ -> true | _ -> false)

let isNumberAdjacentToSymbol symbols (x,y) =
    let neighbors = [
        (x-1, y-1); (x, y-1); (x+1, y-1);
        (x-1, y);             (x+1, y);
        (x-1, y+1); (x, y+1); (x+1, y+1);
    ]

    let symbolSet = Set.ofList symbols
    let neighborSet = Set.ofList neighbors

    (Set.intersect symbolSet neighborSet)
    |> Set.isEmpty
    |> not

let getValidNumbers () = 
    getNumbers ()
    |> List.filter 
        (fun (col, nr) -> 
                      col
                      |> List.filter 
                        (fun (x,y) -> 
                            isNumberAdjacentToSymbol (getSymbolCoordinates ()) (x,y))
                      |> List.isEmpty
                      |> not
                 )


getSymbols()

let result = 
    getValidNumbers ()
    |> List.map snd 
    |> List.sumBy (fun n -> match n with | Bundle.Number n -> n | _ -> 0 )

result |> printfn "Result %i"


