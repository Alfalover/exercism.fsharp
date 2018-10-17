module BeerSong
open System


let ToTitleCase (text:string):string = 
            text  |> Seq.toList 
                 |> Seq.mapi(fun i e -> i |> function 
                                             | 0 -> Char.ToUpper(e)
                                             | _ -> e)
                 |> Array.ofSeq 
                 |> String

 
let bottlesText number = match number with
                                | 0 -> "no more bottles"
                                | 1 -> "1 bottle"
                                | x -> sprintf "%d bottles" x 
              
let takeText number = match number with 
                        | 1 -> "Take it down"
                        | _ -> "Take one down"

let watch bottles =  
        bottlesText bottles 
        |> function 
           | b -> sprintf "%s of beer on the wall, %s of beer." (ToTitleCase b) b
           
let take bottles =  bottles |> function 
                               | 0 -> sprintf "Go to the store and buy some more, 99 bottles of beer on the wall."
                               | s -> sprintf "%s and pass it around, %s of beer on the wall." (takeText s) (bottlesText (s-1))

let rec sing bottles song takeDown =
               takeDown |> function
                        | 0 -> song
                        | 1 -> take bottles::watch bottles::song
                        | t -> t-1 |> sing (bottles-1) (""::(take bottles)::(watch bottles)::song) 

                   
let recite (startBottles: int) (takeDown: int) = sing startBottles [] takeDown  |> List.rev
