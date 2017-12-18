#time
    
let input = System.IO.File.ReadAllLines "day13.txt"
let example_1 = 
    [|
       "0: 3"
       "1: 2"
       "4: 4"
       "6: 4"
    |]

let solve (input: string []) = 
    
    let layers = 
        input 
        |> Array.map( fun line -> 
            let values = line.Split ([| ':'; ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
            int values.[0], int values.[1]
        ) 
    
    
    let inline sumBy (f : int -> int -> int) delay =
        layers 
        |> Array.sumBy (fun (d, r) -> 
            if (d + delay) % (2 * r - 2) = 0 then 
                f d r else 0
        )

    sumBy (*) 0, Seq.initInfinite (fun i -> i, sumBy (+) i) |> Seq.pick (fun (i, s) -> if s = 0 then Some i else None)



module PartI = 
    
    let answers _ = 
        printfn "... Part I ... "
        solve example_1 |> fun (severity, _) -> severity |> printfn "Example 1: %A" 
        solve input |> fun (severity, _) -> severity |> printfn "Answer: %A" 

PartI.answers() 

module PartII = 

    let answers _ = 
        printfn "... Part II ... "
        solve example_1 |> fun (_, smallestDelay) -> smallestDelay |> printfn "Example 1: %d" 
        solve input |> fun (_, smallestDelay) -> smallestDelay |> printfn "Answer: %d" 

PartII.answers() 
