module Bob

open System

let isAlphabetic = String.exists(fun c -> Char.IsLetter(c))

// Performance ?
let isUpperCase  = String.forall(fun c -> not (Char.IsLetter(c)) || Char.IsUpper(c))
let isUpperCase2 (a:string) = a.ToUpper() = a

let isQuestion (a:string) = a.EndsWith("?")

// Perfomance ?
let isEmpty = String.forall(fun c -> Char.IsControl(c) || Char.IsWhiteSpace(c))
let isEmpty2 (a:string) = a.Replace("\t","").Replace(" ","").Length = 0

let response (input: string): string = match input.Trim() with
                                        | a  when isEmpty a -> "Fine. Be that way!"
                                        | a  when isAlphabetic a && isUpperCase a && isQuestion a -> "Calm down, I know what I'm doing!"
                                        | a  when isAlphabetic a && isUpperCase a  -> "Whoa, chill out!"
                                        | a  when isQuestion a -> "Sure." 
                                        | _ -> "Whatever."
