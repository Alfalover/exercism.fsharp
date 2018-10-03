module SumOfMultiples

let multiples (number,upperLimit) = Seq.initInfinite(fun i -> i*number) |> Seq.takeWhile(fun x -> x < upperLimit)

let sum (numbers: int list) (upperBound: int): int = numbers |> List.map(fun x -> multiples(x,upperBound)) 
                                                             |> Seq.concat |> Seq.distinct |> Seq.sum