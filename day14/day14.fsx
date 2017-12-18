#time
    
let input = "hwlqcszp"
let example_1 = "flqrgnkx"


let solve input = 
    //TBD need to do this ... 
    input 

module PartI = 
    
    let answers _ = 
        printfn "... Part I ... "
        solve example_1  |> printfn "Example 1: %A" 
        solve input |> printfn "Answer: %A" 

PartI.answers() 

module PartII = 

    let answers _ = 
        printfn "... Part II ... "
        solve example_1  |> printfn "Example 1: %A" 
        solve input  |> printfn "Answer: %A" 

PartII.answers() 
