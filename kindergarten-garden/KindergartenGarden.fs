module KindergartenGarden

type Plant =  Clover | Grass | Radishes | Violets | None

let children = ["Alice";
                "Bob";
                "Charlie";
                "David";
                "Eve";
                "Fred";
                "Ginny";
                "Harriet";
                "Ileana";
                "Joseph";
                "Kincaid";
                "Larry"]

let charPlantMap x = x |> function
                          | 'V' -> Plant.Violets
                          | 'R' -> Plant.Radishes
                          | 'C' -> Plant.Clover
                          | 'G' -> Plant.Grass
                          |  _  -> Plant.None

let plants (diagram:string) student = 
            let sI = children |> List.findIndex(fun n -> n = student)
            diagram.Split '\n' 
                |> Seq.ofArray
                |> Seq.map(fun x -> x |> Seq.skip(sI*2) |> Seq.take(2))
                |> Seq.concat
                |> Seq.map(fun c -> charPlantMap c)
                |> List.ofSeq


