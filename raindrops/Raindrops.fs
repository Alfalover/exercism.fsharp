module Raindrops


let convert number =
    [3;5;7] 
    |> List.filter(fun x -> number%x = 0)
    |> List.map( function 3 -> "Pling"
                         |5 -> "Plang"
                         |7 -> "Plong")
    |> function 
        | [] -> string number
        | sounds ->  String.concat "" sounds

