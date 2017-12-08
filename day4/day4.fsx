#time

open System.IO



//Part I
module PartI = 
    let example_1 = [|"aa"; "bb"; "cc"; "dd"; "ee"|] 
    let example_2 = [|"aa"; "bb"; "cc"; "dd"; "aa" |]
    let example_3 = [| "aa"; "bb"; "cc"; "dd"; "aaa" |]
    let input = File.ReadAllLines "day4.txt"
    let solution_1(input) =     
        let isValid (password: string) = 
            password.Split(' ')
            |> Array.countBy id 
            |> Array.exists (fun x -> (snd x) > 1 )
            |> not
        
        input 
        |> Seq.sumBy(fun password -> if isValid password then 1 else 0)   

    let answers _ = 
        printfn "... Part I ..."
        solution_1(example_1) |> printfn "Example 1: %d"  
        solution_1(example_2) |> printfn "Example 2: %d"  
        solution_1(example_3) |> printfn "Example 3: %d"  
        solution_1(input)     |> printfn "Input: %d"  

PartI.answers()


// Part II
module PartII = 

    let input = File.ReadAllLines "day4.txt"
    let example_1 = [|"abcde"; "fghij"|]
    let example_2 = [|"abcde"; "xyz"; "ecdab"|]
    let example_3 = [|"a"; "ab"; "abc"; "abd"; "abf"; "abj"|]
    let solution_2(input) =     
        let isValid (password: string) = 
            password.Split(' ')
            |> Array.map ( Seq.sort >> System.String.Concat )
            |> Array.countBy id 
            |> Array.exists (fun x -> (snd x) > 1 )
            |> not
        
        input 
        |> Seq.sumBy(fun password -> if isValid password then 1 else 0)   

    let answers _ = 
        printfn "... Part II ..."
        solution_2(example_1) |> printfn "Example 1: %d"  
        solution_2(example_2) |> printfn "Example 2: %d"  
        solution_2(example_3) |> printfn "Example 3: %d"  
        solution_2(input)     |> printfn "Input: %d"  

PartII.answers()

