module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let roster (school: School): (int * string list) list =
                            school |> Map.toList

let grade (number: int) (school: School): string list =
                            school |> Map.tryFind(number)
                                   |> Option.defaultValue [] 
                                                              
let add (student: string) (number: int) (school: School): School =      
                            school |> Map.add number (student::(grade number school) |> List.sort)
                                                          