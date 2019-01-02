module TreeBuilding

open System
open System.Diagnostics

type Record = { RecordId: int; ParentId: int }
type Tree = 
    | Branch of int * Tree list
    | Leaf of int

let recordId t = 
    match t with
    | Branch (id, c) -> id
    | Leaf id -> id

let isBranch t = 
    match t with
    | Branch (id, c) -> true
    | Leaf id -> false

let children t = 
    match t with
    | Branch (id, c) -> c
    | Leaf id -> []


let buildTree records = 
    records |> List.sortBy (fun x -> x.RecordId) 
            |> function 
                | [] -> failwith "Empty input"
                | ra::__ when ra.ParentId <> 0 -> failwith "Root node is invalid"
                | rb::__ when rb.RecordId <> 0 -> failwith "Root node is invalid"
                | __::rl when rl 
                              |> List.exists (fun  r -> r.RecordId <> 0 && (r.ParentId > r.RecordId || r.ParentId = r.RecordId)) 
                              -> failwith "Nodes with invalid parents"
                | rl     when rl 
                              |> List.pairwise
                              |> List.map (fun x -> Debug.WriteLine x ;x)
                              |> List.exists (fun (a,b) -> b.RecordId <> (a.RecordId+1)  )
                              -> failwith "Non-continuous list"
                | r2 -> 
                    
                           let rec helper k list =
                               list |> Map.tryFind k
                                      |> function 
                                         | Some i -> Branch(k, i |> List.map (fun x -> list |> helper x))
                                         | None   -> Leaf k                                    
     
                           r2 |> List.map (fun x -> x |>
                                                    function
                                                    | u when u.RecordId = 0 -> (-1,u.RecordId)
                                                    | u -> (u.ParentId,u.RecordId))

                              |> List.groupBy fst 
                              |> List.map (fun (x, y) -> (x, List.map snd y))
                              |> Map.ofSeq
                              |> helper 0
