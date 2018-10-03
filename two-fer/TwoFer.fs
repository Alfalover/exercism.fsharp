module TwoFer

// Fist attempt
let twoFer0 (input: string option): string =  "One for " + (match input with 
                                                            | Some x -> x
                                                            | None -> "you") + ", one for me."

// Second attempt
let twoFer (input: string option): string = sprintf "One for %s, one for me." (Option.defaultValue "you" input)
                                                           