module QueenAttack

let create (qx,qy) =  
        let inside x = x >= 0 && x <= 7
        inside qx && inside qy

let canAttack (q1x,q1y) (q2x,q2y) = 
               q1x = q2x || q1y = q2y || (q1x-q2x)/(q1y-q2y) |> abs = 1

