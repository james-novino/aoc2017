#time
    
let input = "873,583"
let example_1 = "65,8921"



let modNumber = 2147483647UL

let compare ((a : uint64), (b : uint64)) = (65535UL &&& a) = (65535UL &&& b)

module PartI = 
    
    let generator factor (start : int) =
        let rec loop prev =
            let next = (uint64 factor * uint64 prev) % modNumber
            seq {
                yield next
                yield! loop next
            }
        loop (uint64 start)

    let judge aStart bStart n =
        let a = generator 16807 aStart
        let b = generator 48271 bStart
        Seq.zip a b
        |> Seq.take n
        |> Seq.filter compare

    let solve (input:string) =
        input.Split([|','|])
        |> Array.map int |> fun x -> x.[0], x.[1]
        |> fun (A,B) -> judge A B 40000000
        |> Seq.length

    let answers _ = 
        printfn "... Part I ... "
        solve example_1  |> printfn "Example 1: %A" 
        solve input |> printfn "Answer: %A" 

PartI.answers() 

module PartII = 

    let generator factor (start : int) (mult : int)  =
        let rec loop prev =
            let next = (uint64 factor * uint64 prev) % modNumber
            if next % uint64 mult = 0UL then
                seq {
                    yield next
                    yield! loop next
                }
            else loop next
        loop (uint64 start)

    let judge aStart bStart n =
        let a = generator 16807 aStart 4
        let b = generator 48271 bStart 8
        Seq.zip a b
        |> Seq.take n
        |> Seq.filter compare

    let solve (input:string) = 
        input.Split([|','|])
        |> Array.map int |> fun x -> x.[0], x.[1]
        |> fun (A,B) -> judge A B 5000000
        |> Seq.length

    let answers _ = 
        printfn "... Part II ... "
        solve example_1  |> printfn "Example 1: %A" 
        solve input  |> printfn "Answer: %A" 

PartII.answers() 
