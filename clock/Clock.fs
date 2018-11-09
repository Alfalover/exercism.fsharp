module Clock

// real modulus operation % operator is remainder
// don't work for neg numbers
let md m x = 
     x - m * floor (x / m)

let create hours minutes =           
          (  float hours + floor (float minutes/60.0) 
             |> md 24.0 |> int,
             float minutes 
             |> md 60.0 |> int)

let add minutes (hh,mm) = create hh (mm + minutes)

let subtract minutes (hh,mm) = create hh (mm - minutes)

let display (hh,mm) = sprintf "%02d:%02d" hh mm