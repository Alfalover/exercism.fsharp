module Raindrops

let valids = [3;5;7] 

let factors number = valids |> Seq.filter(fun x -> number%x = 0)

let replace number = factors number |> Seq.map(fun x -> match x with
                                                         |3 -> "Pling"
                                                         |5 -> "Plang"
                                                         |7 -> "Plong")

let convert (number: int): string = if Seq.isEmpty (replace number) then sprintf "%d" number else  String.concat "" (replace number)  