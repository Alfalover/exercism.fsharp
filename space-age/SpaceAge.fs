module SpaceAge

type Planet = 
    | Earth 
    | Mercury
    | Venus
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune

let earthOrbitSeconds = 31557600.0

let orbitSeconds planet =  match planet with
                                | Earth     -> earthOrbitSeconds 
                                | Mercury   -> 0.2408467  * earthOrbitSeconds
                                | Venus     -> 0.61519726 * earthOrbitSeconds
                                | Mars      -> 1.8808158  * earthOrbitSeconds
                                | Jupiter   -> 11.862615  * earthOrbitSeconds
                                | Saturn    -> 29.447498  * earthOrbitSeconds
                                | Uranus    -> 84.016846  * earthOrbitSeconds
                                | Neptune   -> 164.79132  * earthOrbitSeconds

let age (planet: Planet) (seconds: int64): float =  float seconds/ orbitSeconds planet 