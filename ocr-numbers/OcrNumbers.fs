module OcrNumbers

open System

let numbers = [" _ | ||_|   "; // 0
               "     |  |   "; // 1
               " _  _||_    "; // 2
               " _  _| _|   "; // 3
               "   |_|  |   "; // 4
               " _ |_  _|   "; // 5
               " _ |_ |_|   "; // 6
               " _   |  |   "; // 7
               " _ |_||_|   "; // 8
               " _ |_| _|   "; // 9
               ]    
                  

let matchChar text  = numbers |> List.tryFindIndex(fun x -> x = text)
                                 |> function 
                                    | Some x -> Some(string x)
                                    | None   -> Some("?")

let getRows input = input |> Seq.ofList
                          |> Seq.chunkBySize 4
                          |> Seq.filter(fun x -> x.Length = 4)    
                          
let getCols input  = input |> Seq.map(fun x -> x 
                                              |> Seq.ofArray
                                              |> Seq.map(fun x -> x |> Seq.chunkBySize 3 
                                                                    |> Seq.indexed))
                           |> Seq.filter(fun x -> x 
                                                 |> Seq.forall(fun e -> e
                                                                        |> Seq.forall (fun i -> 
                                                                                (snd i).Length = 3) ))

let renderRow input = input |> Seq.map(fun x -> x 
                                                   |> Seq.collect(fun y -> y|> function
                                                                               | Some x -> x
                                                                               | None -> "")
                                                   |> String.Concat)

let convert   (input:string list) = 
                      
                    input |> getRows          
                          |> getCols
                          |> Seq.map( fun x-> x 
                                             |> Seq.concat
                                             |> Seq.groupBy (fun (x,y) -> x) 
                                             |> Seq.map(fun z -> snd z
                                                                    |> Seq.map(fun u -> snd u)
                                                                    |> Seq.concat
                                                                    |> String.Concat
                                                                    |> matchChar
                                                               ))
                          |> renderRow 
                          |> List.ofSeq
                          |> function
                             |  [] -> None
                             |  x -> Some (x |> String.concat(","))
