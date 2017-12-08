#time 

module Question1 =
    // Day 1, Question 1
    let input = "1122"

    let xs = 
        input 
        |> List.ofSeq 
        |> List.map (fun x -> x.ToString()|>int)

    let sumeq a b = if a = b then b else 0

    let rec sum acc prev (xs:int list) =
        match xs with
        | [] -> acc
        | h::t ->
            let newacc = (sumeq prev h) + acc
            sum newacc h t

    let getCaptcha () =
        let last = xs.[xs.Length-1]
        sum 0 last xs

Question1.getCaptcha () |> printfn "Question 1 - %d"

module Question2 =

    // Day 1, Question 2
    let input = "1122"
    let xs = input |> List.ofSeq |> List.map (fun x -> int x - int '0')

    let sum (xs:int list) =
        let len = xs.Length
        let sumeq a b = if a = b then b else 0
        let rec _sum acc steps (xs:int list) =
            match steps, xs with
            | 0, _ -> acc
            | _, [] -> acc
            | _, h::t ->
                let total = sumeq h (xs.[len/2])
                _sum (acc + total) (steps - 1) (t @ [h])
        xs |> _sum 0 (len)

Question2.sum Question2.xs |> printfn "Question 2 - %d"