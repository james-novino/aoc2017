<Query Kind="FSharpProgram">
  <Reference>&lt;MyDocuments&gt;\Code\gambit\Gambit.Core\servicebin\EventStore.ClientAPI.dll</Reference>
  <Reference>&lt;MyDocuments&gt;\Code\gambit\Gambit.Core\servicebin\FSharp.Core.dll</Reference>
  <Reference>&lt;MyDocuments&gt;\Code\gambit\Gambit.Core\servicebin\Gambit.EventStore.dll</Reference>
  <Reference>&lt;MyDocuments&gt;\Code\gambit\Gambit.Core\servicebin\Marvel.Consul.dll</Reference>
  <Reference>&lt;MyDocuments&gt;\Code\gambit\Gambit.Core\servicebin\Marvel.dll</Reference>
  <Reference>&lt;MyDocuments&gt;\Code\gambit\Gambit.Core\servicebin\Marvel.EventStore.dll</Reference>
  <Reference>&lt;MyDocuments&gt;\Code\gambit\Gambit.Core\servicebin\Marvel.Json.dll</Reference>
  <Reference>&lt;MyDocuments&gt;\Code\gambit\Gambit.Core\servicebin\Microsoft.ServiceBus.dll</Reference>
  <Reference>&lt;MyDocuments&gt;\Code\gambit\Gambit.Core\servicebin\Newtonsoft.Json.dll</Reference>
  <Reference>&lt;MyDocuments&gt;\Code\gambit\Gambit.Core\servicebin\NLog.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Collections.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.dll</Reference>
</Query>

// Input
let toInt (s : string[]) : int[] = Array.map (fun e -> int e) s
let input = System.IO.File.ReadAllLines "C:\\code\\aoc2017\\day2\\day2.txt" 
            |> Array.map (fun x -> x.Split [|'\t'|])
            |> Array.map toInt

// Part 1
let bigDiff (arr : int[]) : int = 
    (Array.max arr) - (Array.min arr)

printfn "part 1: %A" (Array.fold (fun acc elem -> acc + bigDiff elem) 0 input)

//// Part 2
let isDiv (x:int) (arr:int[]) : bool = 
    Array.exists (fun i -> x%i = 0 && x <> i|| i%x = 0 && x <> i) arr
let ifDiv i arr = 
    if (isDiv i arr) then i else 0
let divs (arr : int[]) : int[] = 
    Array.map (fun e -> ifDiv e arr) arr |> Array.filter (fun e -> e <> 0)
let divbws (arr:int[]):int = 
    if arr.[0] > arr.[1] then arr.[0]/arr.[1] else arr.[1]/arr.[0]

printfn "part 2: %A" (Array.fold (fun acc elem -> acc + divbws (divs elem)) 0 input)