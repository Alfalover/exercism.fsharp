module old

exception NotExactChangeError of unit



let minimumCoin coins = coins 
                         |> List.min

let validCoins coins value = coins 
                               |> List.filter(fun x -> x <= value)

let maximumCoin coins value = coins 
                               |> List.filter(fun x -> x <= value)
                               |> List.tryLast

let previousCoin coins value = coins
                                |> List.filter(fun x -> x <= value)
                                |> List.rev
                                |> List.skip 1
                                |> List.tryHead


let rec changeSolver coins (value:int)  = 
                                value |> function 
                                         | 0 -> []
                                         | x -> (maximumCoin coins value) 
                                                 |> function 
                                                     | None -> raise(NotExactChangeError())
                                                     | Some coin -> (value - coin)
                                                                 |> function 
                                                                    | 0 -> [coin]
                                                                    | y when y >= (minimumCoin coins) ->
                                                                        let res = coin::(changeSolver coins (value-coin)) 
                                                                        printfn "%s" (res.ToString())
                                                                        res
                                                                    | y -> 
                                                                        let prev = previousCoin coins coin
                                                                        prev |> function 
                                                                                |Some pr -> let res = pr::(changeSolver coins (value-pr)) 
                                                                                            printfn "%s" (res.ToString())
                                                                                            res
                                                                                | None -> raise(NotExactChangeError())
                                                                    
let runRule coins target =
    try
       Some ( (changeSolver coins target)|> List.rev )
    with
     | NotExactChangeError() ->
             None
 
let findFewestCoinsB (coins:int list) target =   [(runRule (coins |> List.skip 1 ) target);
                                                 (runRule coins target) ;
                                                 (runRule (coins |> List.rev |> List.skip 1 |> List.rev) target)
                                                 ]
                                                 |> List.filter(fun x -> x <> None)
                                                 |> function
                                                    | [] -> None
                                                    | x ->  x |> List.sortBy(fun l -> l.Value.Length)
                                                              |> List.head
 


