(* counter state *)
type state = {mutable value : int; mutable step : int; mutable continue : bool}

(* initial counter state *)
let counter = {value = 0; step = 0; continue = true}

(* actions *)
let incr v = v + 1

let decr v = v - 1

let change f s =
  s.value <- f s.value ;
  s.step <- s.step + 1 ;
  s

let stop s =
  s.continue <- false ;
  s
