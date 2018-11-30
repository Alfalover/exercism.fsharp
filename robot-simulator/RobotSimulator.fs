module RobotSimulator

type Direction = North = 0 | East = 1 | South = 2 | West = 3
type Position = int * int
type Robot = { direction: Direction; position: Position }

let md b a =  (a % b + b) % b

let create direction position = {direction = direction ; position = position}

let stepLeft robot =
            {direction= enum<Direction>( md 4 ((int robot.direction)-1)) ; position = robot.position}
                       
let stepRight robot =
            {direction= enum<Direction>( md 4 ((int robot.direction)+1)) ; position = robot.position}

let stepAdvance robot =
                       let x,y = robot.position
                       match robot.direction with 
                       | Direction.North -> {direction= robot.direction ; position = (x,y+1)}
                       | Direction.East ->  {direction= robot.direction ; position = (x+1,y)}
                       | Direction.South -> {direction= robot.direction ; position = (x,y-1)}
                       | Direction.West ->  {direction= robot.direction ; position = (x-1,y)}

let rec accum func acc input  = 
                                input |> function 
                                         | [] -> acc
                                         | x::xs -> accum func (func x acc) xs
                                               
let movement command robot =  command |> function 
                                         | 'R' -> stepRight robot
                                         | 'L' -> stepLeft robot
                                         | 'A' -> stepAdvance robot
                                         | x  ->  invalidArg  (string x) "Invalid command" 

let move instructions robot = instructions 
                                |> List.ofSeq
                                |> accum movement robot 