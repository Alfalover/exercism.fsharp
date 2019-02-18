module Change
open System

exception NotExactChangeError of unit



let minimumCoin coins = coins 
                         |> List.min

let validCoins coins value = coins 
                               |> List.sortBy(fun x -> value/x)
                               |> List.filter(fun x -> value/x >= 1)
                               |> List.indexed
                               |> List.takeWhile(fun (i,x) -> i < 2 )
                               |> List.map(fun (i,x) -> x)

let rec changeSolverB (coins:int list) (value:int)  = 
        value |> function 
                 | 0 -> [Some([])]
                 | z when z < 0 -> [None]
                 | x -> (validCoins coins value) 
                        |> List.map(fun coin -> (value - coin)
                                                   |> changeSolverB coins 
                                                   |> List.choose(fun x -> x)
                                                   |> List.map(fun j -> Some(coin::j))
                                   )
                        |> List.collect(fun sm -> sm)
                                                                    
let findFewestCoins (coins:int list) target =    changeSolverB coins target
                                                 |> List.filter(fun x -> x <> None)
                                                 |> function
                                                    | [] -> None
                                                    | x ->  x |> List.sortBy(fun l -> l.Value.Length)
                                                              |> List.head 
                                                              |> function 
                                                                 | None -> None
                                                                 | Some x -> Some(x |> List.rev)
                                                             
