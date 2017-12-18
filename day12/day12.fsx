#time
open System.IO
open System.Text
open System

let rec traverse (visited : Set<int>) (current : int)  (graph: Map<int, Set<int>>) : Set<int> =
    if visited |> Set.contains current then
        Set.empty
    else
        let visited' = visited.Add current
        graph.[current] + (graph.[current] 
            |> Set.map (fun current -> traverse visited' current graph) 
            |> Set.unionMany)

let rec calcTotal (graph: Map<int, Set<int>>) (vertices : Set<int>)  =
    if Set.isEmpty vertices then 
        0 
    else 
        1 + calcTotal graph (vertices - (traverse Set.empty (vertices |> Set.minElement) graph))
    
let input = System.IO.File.ReadAllLines "day12.txt"
let example_1 = 
    [|
        "0 <-> 2"
        "1 <-> 1"
        "2 <-> 0, 3, 4"
        "3 <-> 2, 4"
        "4 <-> 2, 3, 6"
        "5 <-> 6"
        "6 <-> 4, 5"
    |]

let solve (input: string []) = 
        
        let graph = 
            input 
            |> Array.map( fun line -> 
                let x = line.Split([| ' '; ',' |]) |> Array.filter(fun str -> str <> "")
                int x.[0], x.[2..] |> Array.map int |> Set.ofArray
            ) 
            |> Map.ofArray
        
        graph
        |> traverse Set.empty 0 
        |> Set.count, graph
        |> Map.toList 
        |> List.map fst
        |> Set.ofList
        |> calcTotal graph



module PartI = 
    
    let answers _ = 
        printfn "... Part I ... "
        solve example_1 |> fun (total, _) -> total |> printfn "Example 1: %d" 
        solve input |> fun (total, _) -> total |> printfn "Answer: %A" 

PartI.answers() 

module PartII = 

    let answers _ = 
        printfn "... Part II ... "
        solve example_1 |> fun (_, groups) -> groups |> printfn "Example 1: %d" 
        solve input |> fun (_, groups) -> groups |> printfn "Answer: %d" 

PartII.answers() 
