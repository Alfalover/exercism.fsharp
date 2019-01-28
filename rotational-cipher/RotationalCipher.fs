module RotationalCipher

let modulus n m = ((n % m) + m) % m

let ShiftCharacter offset shiftKey char = (modulus ((char-offset) + shiftKey) 26)  + offset

let rotate shiftKey (text:string) = 
    text |> Seq.map (fun x ->  x |> function 
                                    | a when a >= 'a' && a <= 'z' ->  ShiftCharacter (int 'a') shiftKey (int a)  
                                    | a when a >= 'A' && a <= 'Z' ->  ShiftCharacter (int 'A') shiftKey (int a) 
                                    | b -> int b 
                                 |> char 
                                 |> string ) 
         |> String.concat ""