#time
open System.IO
open System.Text
open System
open System.Text.RegularExpressions

type Tower =
    { Name: string; Weight: int; Stacks: Tower list }
    with
        member x.TotalWeight =
            x.Weight + (x.Stacks |> List.sumBy (fun s -> s.TotalWeight))
        member x.Balanced =
            x.Stacks 
            |> Seq.ofList 
            |> Seq.distinctBy(fun s -> s.TotalWeight) 
            |> Seq.length 
            |> ((=) 1)

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None


let rec buildTower stubs rootName =
    let name, weight, children = stubs |> Array.find (fun (name,_,_) -> rootName = name)
    {
        Name = name;
        Weight = weight;
        Stacks = children |> List.ofArray |> List.map (buildTower stubs)
    }

let private findRoot stubs =
    stubs |> Array.map (fun (name,_,_) ->
        name,
        stubs |> Array.sumBy (fun (_,_,children) ->
            if children |> Array.exists ((=) name) then 1 else 0
        )
    )
    |> Array.sortBy snd
    |> Array.toList
    |> List.head
    |> fst

//Part I
module PartI = 
      
    let stubs =
         File.ReadAllLines "day7.txt"
        |> Array.map (
            function
            | Regex @"([a-z]+) \((\d+)\)(?: -> )?(.*)" [name; weight; childList] ->
                name,
                int weight,
                childList.Split([|','|]) |> Array.choose(fun s -> match s.Trim() with "" -> None | trimmed -> Some trimmed)
            | _ -> failwith "Invalid input"
        )

    let tower = buildTower stubs (findRoot stubs)
    let answers _ = 
        printfn "... Part I ..."
        printfn "Bottom Program: %s" tower.Name
        
PartI.answers()


module PartII = 

    let rec findBalance (tower:Tower) shouldWeigh =

        if tower.TotalWeight <> shouldWeigh && tower.Balanced then
            tower.Weight + (shouldWeigh - tower.TotalWeight)
        elif not tower.Balanced then
            let children =
                tower.Stacks
                |> List.map (fun s ->
                    s,
                    tower.Stacks
                    |> List.exists(fun ss -> s.Name <> ss.Name && ss.TotalWeight = s.TotalWeight))
            // What *should* all the children weigh?
            let childrenShouldWeigh = (children |> List.find snd |> fst).TotalWeight

            // Which child doesn't
            let unbalancedChild = children |> List.find (snd >> not) |> fst

            findBalance unbalancedChild childrenShouldWeigh
        else tower.Weight


    let stubs =
         File.ReadAllLines "day7.txt"
        |> Array.map (
            function
            | Regex @"([a-z]+) \((\d+)\)(?: -> )?(.*)" [name; weight; childList] ->
                name,
                int weight,
                childList.Split([|','|]) |> Array.choose(fun s -> match s.Trim() with "" -> None | trimmed -> Some trimmed)
            | _ -> failwith "Invalid input"
        )
    let tower = buildTower stubs (findRoot stubs)
    let balance = (findBalance tower tower.TotalWeight)
    let answers _ = 
        printfn "... Part II ..."
        printfn "Balance: %d" balance

PartII.answers()