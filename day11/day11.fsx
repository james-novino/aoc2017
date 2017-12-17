open System.Web.Services.Description
#time
open System.IO
open System.Text
open System


type Direction = N | NW | SW | S | SE | NE
    with
        static member FromString =
            function
            | "n" -> N 
            | "nw" -> NW 
            | "ne" -> NE
            | "s" -> S 
            | "sw" -> SW 
            | "se" -> SE
            | _ -> failwith "invalid input"

        static member Angle direction =
            match direction with // A hexigon has 360 degrees /6 = 60 deggrees per side
            | NE -> 30.0
            | N -> 90.0
            | NW -> 150.0
            | SW -> 210.0
            | S -> 270.0
            | SE -> 330.0
            |> (*) (Math.PI / 180.0)

let move (x,y) direction = 
    x + Math.Cos(Direction.Angle direction), y + Math.Sin(Direction.Angle direction)

// Calculate the Euclidean distance between two points
let calcDistance (x1,y1) (x2,y2) = Math.Sqrt((x1-x2)**2.0 + (y1-y2)**2.0) 

// Calculate the number of steps, between the origin and the target point
let rec find home (x,y) =
    if (calcDistance home (x,y)) < 0.5 then 0
    else
        let dir = 
            [N; NW; NE; S; SW; SE] 
            |> List.minBy(move (x,y) >> calcDistance home)
        1 + (find home (move (x,y) dir))


let input = System.IO.File.ReadAllText "day11.txt"

module PartI = 
    
    let example_1 = "ne,ne,ne" 
    let example_2 = "ne,ne,sw,sw"
    let example_3 = "ne,ne,s,s"
    let example_4 = "se,sw,se,sw,sw"


    let solve (input: string ) =

        let home = 0.0, 0.0

        input.Trim().Split([|','|]) 
        |> Array.map Direction.FromString
        |> Array.scan move home
        |> Array.last 
        |> find home


    let answers _ = 
        printfn "... Part I ... "
        solve example_1 |> printfn "Example 1: %d" 
        solve example_2 |> printfn "Example 2: %d"
        solve example_3 |> printfn "Example 3: %d" 
        solve example_4 |> printfn "Example 4: %d" 
        solve input |> printfn "Answer: %d" 

PartI.answers() 

module PartII = 
    let example_1 = "ne,ne,ne" 
    let example_2 = "ne,ne,sw,sw"
    let example_3 = "ne,ne,s,s"
    let example_4 = "se,sw,se,sw,sw"
    
    let solve (input: string ) =

        let home = 0.0, 0.0

        input.Trim().Split([|','|])
        |> Array.map Direction.FromString
        |> Array.scan move home
        |> Array.map (find home) 
        |> Seq.max

    let answers _ = 
        printfn "... Part II ... "
        solve example_1 |> printfn "Example 1: %d" 
        solve example_2 |> printfn "Example 2: %d" 
        solve example_3 |> printfn "Example 3: %d" 
        solve example_4 |> printfn "Example 4: %d" 
        solve input |> printfn "Answer: %d" 

PartII.answers() 
