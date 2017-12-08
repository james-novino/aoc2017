#time
open System.IO

let (~+) value = 
    value + 1

let clone (array: int[]) = array.Clone () :?> int array

//Part I
module PartI = 
    let example_1 = [|0;2;7;0|] 
    let input = File.ReadAllText "day6.txt" |> fun memory -> memory.Split('\t') |> Array.map int
    let rec permute banks previous =
        let index, free = 
            banks 
            |> Array.mapi (fun i v -> i,v) 
            |> Array.maxBy snd 
        banks.[index] <- 0

        for i = 1 to free do
            let nextIndex = (index + i) % banks.Length
            banks.[nextIndex] <- banks.[nextIndex] + 1

        if previous |> Map.containsKey banks then previous.Count + 1            
        else
            previous
            |> Map.add (clone banks) previous.Count
            |> permute banks

    let answers _ = 
        printfn "... Part I ..."
        permute example_1 Map.empty |> printfn "Example 1: %d"  
        permute input Map.empty |> printfn "Input: %d" 
        
PartI.answers()


module PartII = 
    let example_1 = [|0;2;7;0|] 
    let input = File.ReadAllText "day6.txt" |> fun memory -> memory.Split('\t') |> Array.map int
    let rec permute banks previous =
        let index, free = 
            banks 
            |> Array.mapi (fun i v -> i,v) 
            |> Array.maxBy snd 
        banks.[index] <- 0

        for i = 1 to free do
            let nextIndex = (index + i) % banks.Length
            banks.[nextIndex] <- banks.[nextIndex] + 1

        if previous |> Map.containsKey banks then (previous.Count + 1, previous.Count - previous.[banks])            
        else
            previous
            |> Map.add (clone banks) previous.Count
            |> permute banks

    let answers _ = 
        printfn "... Part I ..."
        permute example_1 Map.empty |> printfn "Example 1: %A"  
        permute input Map.empty |> printfn "Input: %A" 

PartII.answers()
