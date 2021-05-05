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

(* eventuality (<>) property *)
type eventuality_property = {
  mutable satisfy : bool;
  predicate : state -> bool;
  mutable state : int;
}

(* <>state_predicate *)
let liveness_property pred = {satisfy = false; predicate = pred; state = 0}

(* eventually follows (~>) property *)
type eventually_follows_property = {
  mutable conditional_satisfy : bool;
  conditional : state -> bool;
  mutable satisfy : bool;
  predicate : state -> bool;
}

let follows_property cond pred =
  {
    conditional_satisfy = false;
    conditional = cond;
    satisfy = false;
    predicate = pred;
  }
