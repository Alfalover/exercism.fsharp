module Allergies

open System

// TODO: define the Allergen type
[<System.FlagsAttribute>]
type Allergen = Eggs = 1
                | Peanuts = 2
                | Shellfish = 4
                | Strawberries = 8
                | Tomatoes = 16
                | Chocolate = 32
                | Pollen = 64
                | Cats = 128

let allergicTo codedAllergies allergen = (codedAllergies &&& (int allergen) <> 0)

let list codedAllegies = Enum.GetValues(typeof<Allergen>) 
                            |> unbox 
                            |> Seq.cast<Allergen> 
                            |> Seq.filter(fun x -> allergicTo codedAllegies x )
                            |> List.ofSeq



                           