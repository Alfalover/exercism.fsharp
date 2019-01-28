module Minesweeper

let CharToInt c = int (string c)

let IntToChar i = (string i).[0]

let To2DArray d = Array2D.init (List.length d) (String.length d.Head) (fun i j -> d.[i].[j])

let ToStringList d =  {0..(Array2D.length1 d)-1}
                      |> Seq.map(fun i -> d.[i,*] 
                                          |> List.ofArray
                                          |> List.map (fun x -> string x) 
                                          |> String.concat "")
                      |> List.ofSeq


let Scan f d = d |> Array2D.mapi (fun i j v -> match v with 
                                                | '*' -> '*' 
                                                | ' ' -> f d i j  
                                                            |> function 
                                                               | 0 -> ' '           
                                                               | v -> IntToChar v
                                                |  v  -> IntToChar ((CharToInt v) + (f d i j))
                                                )


let flatArray d =   {0..(Array2D.length1 d)-1}
                         |> Seq.map(fun i -> d.[i,*] 
                                             |> List.ofArray)
                         |> List.ofSeq 
                         |> List.concat

let subArray i j d =    let il = max 0 (i-1);
                        let ih = min ((Array2D.length1 d)-1)  (i+1);
                        let jl = max 0 (j-1);
                        let jh = min ((Array2D.length2 d)-1)  (j+1);
                        d.[il..ih,jl..jh] 

let annotate input =  
            
            input |> function 
                     | [] -> []
                     | l  -> l      |> To2DArray 
                                    |> Scan (fun d i j -> d |> subArray i j 
                                                            |> flatArray
                                                            |> List.filter (fun c -> c = '*')
                                                            |> List.length) 
                                    |> ToStringList 
