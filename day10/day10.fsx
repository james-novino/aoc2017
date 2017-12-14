#time
open System.IO
open System.Text
open System

let revSublist nums pos n = 
    let c = Array.copy nums
    for i in [0..n-1] do
        nums.[(pos + i) % nums.Length] <- c.[(pos + n - i - 1) % nums.Length] 

let hash rounds (input:seq<int>) =
    let numbers = [|0..255|] //Needs to be [|0..4|] for the example
    let mutable position = 0
    let mutable skip = 0
    // Think I could do this with a single reduce some how, but not sure excatly how at the moment
    for _ in [1..rounds] do
        for n in input do
            revSublist numbers position n
            position <- position + n + skip
            skip <- skip + 1 
    numbers

let input = System.IO.File.ReadAllText "day10.txt"

module PartI = 
    
    let example_1 = "3,4,1,5" 
    let solve (input:string) = 
        input.Split(',')   
        |> Array.map int
        |> hash 1
        |> Array.take 2
        |> Array.reduce (*)

    let answers _ = 
        printfn "... Part I ... "
        solve example_1 |> printfn "Example 1: %d" 
        solve input |> printfn "Answer: %d" 

PartI.answers() 

module PartII = 
    let example_1 = ""
    let example_2 = "AoC 2017" 
    let example_3 = "1,2,3"
    let example_4 = "1,2,4"
    
    let solve (input: string) = 
        input.ToCharArray()
        |> Array.map (fun c -> Convert.ToByte(c))
        |> (fun x -> Array.append x [|17uy; 31uy; 73uy; 47uy; 23uy|])
        |> Array.map int
        |> hash 64 
        |> Array.chunkBySize 16
        |> Array.map (Array.reduce (^^^))
        |> Array.fold (fun str digit -> str + sprintf "%02x" digit) ""

    let answers _ = 
        printfn "... Part II ... "
        solve example_1 |> printfn "Example 1: %s" 
        solve example_2 |> printfn "Example 2: %s" 
        solve example_3 |> printfn "Example 3: %s" 
        solve example_4 |> printfn "Example 4: %s" 
        solve input |> printfn "Answer: %s" 

PartII.answers() 
