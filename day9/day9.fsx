#load "../helpers.fs"

#time
open System.IO
open System.Text
open System
open Helpers

type GarbageState = NotGarbage | Garbage | Cancelled

type State = 
    {
        position: int
        state: GarbageState
        score: int
        garbage: int 
    }

let step current nextChar =
    match (current.state, nextChar) with
    | (Garbage, '!') -> {current with state = Cancelled}
    | (Garbage, '>') -> {current with state = NotGarbage} 
    | (Garbage, _ )   -> {current with garbage = current.garbage + 1}
    | (Cancelled, _) 
    | (NotGarbage, '<') -> {current with state = Garbage}
    | (NotGarbage, '{') -> {current with position = current.position + 1}
    | (NotGarbage, '}') -> {current with position = current.position - 1; score = current.score + current.position}
    | _ -> current;

let solve = 
    Seq.fold step {position=0; state=NotGarbage; score=0; garbage=0}

let input = System.IO.File.ReadAllText "day9.txt"

module PartI = 
    let example_1 = "{}"
    let example_2 = "{{{}}}"
    let example_3 = "{{},{}}"
    let example_4 = "{{{},{},{{}}}}"
    let example_5 = "{<a>,<a>,<a>,<a>}"
    let example_6 = "{{<ab>},{<ab>},{<ab>},{<ab>}}"
    let example_7 = "{{<!!>},{<!!>},{<!!>},{<!!>}}"
    let example_8 = "{{<a!>},{<a!>},{<a!>},{<ab>}}"

    let solution_1 = solve >> (fun state -> state.score)
    
    let answers _ = 
        printfn "... Part I ... "
        solution_1 example_1 |> printfn "Example 1: %A"
        solution_1 example_2 |> printfn "Example 2: %A"
        solution_1 example_3 |> printfn "Example 3: %A"
        solution_1 example_4 |> printfn "Example 4: %A"
        solution_1 example_5 |> printfn "Example 5: %A"
        solution_1 example_6 |> printfn "Example 6: %A"
        solution_1 example_7 |> printfn "Example 7: %A"
        solution_1 example_8 |> printfn "Example 8: %A"
        solution_1 input |> printfn "Answer: %A" 


PartI.answers() 

module PartII = 
    let example_1 = "<>"
    let example_2 = "<random characters>"
    let example_3 = "<<<<>"
    let example_4 = "<{!>}>"
    let example_5 = "<!!>"
    let example_6 = "<!!!>>"
    let example_7 = """<{o"i!a,<{i<a>"""
    let solution_2 = solve >> (fun state -> state.garbage)
    let answers _ = 
        printfn "... Part II ... "
        solution_2 example_1 |> printfn "Example 1: %A"
        solution_2 example_2 |> printfn "Example 2: %A"
        solution_2 example_3 |> printfn "Example 3: %A"
        solution_2 example_4 |> printfn "Example 4: %A"
        solution_2 example_5 |> printfn "Example 5: %A"
        solution_2 example_6 |> printfn "Example 6: %A"
        solution_2 example_7 |> printfn "Example 7: %A"
        solution_2 input |> printfn "Answer: %A" 

PartII.answers() 
