#time
open System.IO

let (~+) value = 
    value + 1
//Part I
module PartI = 
    let example_1 = [|0;3;0;1;-3|] 
    let input = File.ReadAllLines "day5.txt" |> Array.map int 
    let rec permute step (total:int) (input:int []) =
        let next = input.[step] + step
        input.[step] <- input.[step] + 1
        if next >= input.Length then + total else permute next (+ total) input
    
    let answers _ = 
        printfn "... Part I ..."
        permute 0 0 example_1 |> printfn "Example 1: %d"  
        permute 0 0 input     |> printfn "Input: %d"  

PartI.answers()


//Part I
module PartII = 
    let example_1 = [|0;3;0;1;-3|] 
    let input = File.ReadAllLines "day5.txt" |> Array.map int 

    let rec permute step (total:int) (input:int []) =
        let next = input.[step] + step
        input.[step] <- if input.[step] >= 3 then input.[step] - 1 else input.[step] + 1
        if next >= input.Length then + total else permute next (+ total) input
    let answers _ = 
        printfn "... Part II ..."
        permute 0 0 example_1 |> printfn "Example 1: %d"  
        permute 0 0 input     |> printfn "Input: %d"  

PartII.answers()
