module Accumulate

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list =
                                                           let rec accum func acc input  = 
                                                                                        input |> function 
                                                                                                 | [] -> acc
                                                                                                 | x::xs -> accum func ((func x)::acc) xs
                                                           input |> List.rev |> accum func []         
