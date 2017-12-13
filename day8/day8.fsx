#load "../helpers.fs"

#time
open System.IO
open System.Text
open System
open Helpers

type Operation = 
| Increment 
| Decrement
let compare left condition right =
    match condition with
    | "<" -> left < right
    | "<=" -> left <= right
    | ">" -> left > right
    | ">=" -> left >= right
    | "==" -> left = right
    | "!=" -> left <> right
    | _-> failwith "unknown operation"

type Instruction =
    { 
        Register: string
        Operation: Operation
        Operand: int
        Condition: string*string*int 
    }
    with
        static member Execute registers (instruction: Instruction) =
            // Lookup register value from map
            let getRegister register = 
                registers 
                |> Map.tryFind register
                |> Option.getValueOr 0

            let conditionalRegister, comparison, conditionalValue = instruction.Condition
            
            let reg = getRegister conditionalRegister 
            // Check condition
            if compare reg comparison conditionalValue then
                let newRegValue = 
                    match instruction.Operation with 
                    | Increment ->  
                        (getRegister instruction.Register) + instruction.Operand
                    | _ ->
                        (getRegister instruction.Register) - instruction.Operand

                // Add new value to map
                registers |> Map.add instruction.Register newRegValue
            else registers

let parseList fn (str: string) =
    str.Split([|'\t'; ' '|])
    |> Array.choose (fun e ->
        match e.Trim() with
        | "" -> None
        | e -> Some (fn e))

let solve input =

    let highestValue = Map.toList >> List.map snd >> List.max

    let finalState, finalHighestValue =
        input
        |> Array.map (fun line ->
            let tokens = parseList id line
            {
                Register = tokens.[0]
                Operation = if tokens.[1] = "inc" then Increment else Decrement
                Operand = int tokens.[2]
                Condition = tokens.[4], tokens.[5], int tokens.[6]
            }
        )
        |> Array.fold (fun (registers, maxValue) instruction ->
            // Step 'cpu'
            let newRegisters = instruction |> Instruction.Execute registers
            let newMaxValue = highestValue newRegisters

            // New fold state
            newRegisters,
            if newMaxValue > maxValue then newMaxValue else maxValue
        ) (Map.empty, 0)


    highestValue finalState, finalHighestValue

//Part I
module PartI =
    let answers _ = 
        printfn "... Part I ..."
        System.IO.File.ReadAllLines "day8.txt"
        |> solve |> fst
        |> printfn "Largest Value in Any Register: %d" 


PartI.answers()


module PartII = 
    let answers _ = 

        printfn "... Part II ..."
        System.IO.File.ReadAllLines "day8.txt"
        |> solve |> snd
        |> printfn "Highest Value held in any register: %d"
PartII.answers()