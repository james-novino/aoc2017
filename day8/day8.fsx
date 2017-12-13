#load "../helpers.fs"

#time
open System.IO
open System.Text
open System
open Helpers

type Operation = Inc | Dec


let comparer left condition right =
    match condition with
    | "==" -> (=)
    | "<" -> (<)
    | ">" -> (>)
    | ">=" -> (>=)
    | "<=" -> (<=)
    | "!=" -> (<>)
    | _ -> failwith "unknown condition"
    |> fun comp -> left comp right 

type Comparison = Comparison of string
    with
        static member Compare left (Comparison comparison) right =
            match comparison with
            | "<" -> left < right
            | "<=" -> left <= right
            | ">" -> left > right
            | ">=" -> left >= right
            | "==" -> left = right
            | "!=" -> left <> right
            | _-> failwith "Invalid operator"

type Instruction =
    { Register: string; Operation: Operation; Operand: int; Condition: string*Comparison*int }
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
            if Comparison.Compare (getRegister conditionalRegister) comparison conditionalValue then
                let newRegValue = // Calculate new register value
                    if instruction.Operation = Inc then (getRegister instruction.Register) + instruction.Operand
                    else (getRegister instruction.Register) - instruction.Operand

                // Add new value to map
                registers |> Map.add instruction.Register newRegValue
            else registers

let parseList fn (str: string) =
    str.Split([|'\t'; ' '; '\r'; '\n'|])
    |> Array.choose (fun e ->
        match e.Trim() with
        | "" -> None
        | e -> Some (fn e))

let solve input =

    let highestValue = Map.toList >> List.map snd >> List.max

    let finalState, finalHighestValue =
        input
        |> Array.map (fun line ->
            // Parse line into instruction
            let tokens = parseList id line
            {
                Register = tokens.[0]
                Operation = if tokens.[1] = "inc" then Inc else Dec
                Operand = int tokens.[2]
                Condition = tokens.[4], Comparison tokens.[5], int tokens.[6]
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
        |> solve
        |> printfn "Highest Value: %A" 


PartI.answers()


module PartII = 
    let answers _ = 
        printfn "... Part II ..."

PartII.answers()