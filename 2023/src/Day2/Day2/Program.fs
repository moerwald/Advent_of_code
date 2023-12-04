namespace Day2

module Types =
    type Color = Red | Green | Blue

    type CubesPerColor = {
        Color : Color
        Count : int
    }

    type Id = int

    type CubeSet = CubesPerColor list

    type Game = {
        Id : Id
        CubeSets : CubeSet list
    }

module Common =

    open Types

    let splitAndTrimAllEntries  (splitCharacter: char) (line : string)=
        line.Split(splitCharacter)
        |> Array.map (fun x -> x.Trim())


    let parseReveal (line :string) =
        let colorToCount = line |> splitAndTrimAllEntries ','
        colorToCount
        |> List.ofArray
        |> List.map(fun element -> 
            
            let tmp = element |> splitAndTrimAllEntries ' '
            let color =
                match tmp[1] with 
                | "red" -> Color.Red 
                | "green" -> Color.Green 
                | _ -> Color.Blue

            { 
                Color = color;
                Count = tmp[0] |> int
            }
            )

    let parseSets (line : string) =
        line 
        |> splitAndTrimAllEntries ';'
        |> List.ofArray
        |> List.map parseReveal

    let sumUpSets (set : CubesPerColor list list) =
        let result = 
            set 
            |> List.collect (fun x -> x)
            |> List.groupBy (fun x -> x.Color)
            |> List.map (fun (col, counts) -> 
                {Color = col
                 Count = counts 
                         |> List.map (fun x -> x.Count)
                         |> List.sum })
        result

    let parseGameId (line : string) =
        (line
        |> splitAndTrimAllEntries  ' '
        |> Array.map (fun x -> x.Trim())
        )[1]
        |> int

    let parseGame (line : string) =
        let split = line |> splitAndTrimAllEntries ':'
        {
            Id = parseGameId split[0]
            CubeSets = split[1] |> parseSets  
        }

    let parseGames (line : string list) =
        line |> List.map  parseGame

            
module Part1 =

    open Types

    let getValidGames (valid : CubesPerColor list) (games : Game list)  =
        let getValidBaseOnColor c=
            (valid 
            |> List.filter (fun x -> x.Color = c) 
            |> List.head).Count
        games
        |> List.filter (fun x -> 
            x.CubeSets
            |> List.forall (fun cs -> 
                cs |> List.forall (fun x -> (getValidBaseOnColor x.Color) >= x.Count)
                )
            )


module Part2 =
    open Types

    let getMinNumberOfCubes (game : Game) =
        game.CubeSets
        |> List.collect (fun x -> x)
        |> List.groupBy (fun x -> x.Color)
        |> List.map (fun (c, counts) -> 
            {
                Color = c;
                Count = counts |> List.map (fun x -> x.Count) |> List.max
            })

    let calcPowerOfAllGames (games : Game list) =
            games
            |> List.map getMinNumberOfCubes
            |> List.map (fun x -> x |> List.fold (fun acc el -> acc * el.Count) 1)
            |> List.sum
