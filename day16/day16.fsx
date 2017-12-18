#time
    
let input = System.IO.File.ReadAllText "day16.txt"
let example_1 = "abcde"


module PartI = 
    
    
    let solve input = 
        input
    let answers _ = 
        printfn "... Part I ... "
        solve example_1  |> printfn "Example 1: %A" 
        solve input |> printfn "Answer: %A" 

PartI.answers() 

module PartII = 

    let solve input = 
        input 

    let answers _ = 
        printfn "... Part II ... "
        solve example_1  |> printfn "Example 1: %A" 
        solve input  |> printfn "Answer: %A" 

PartII.answers() 
