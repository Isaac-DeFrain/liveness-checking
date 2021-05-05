(* counter state *)
type state = {
  mutable value : int;
  mutable step : int;
  mutable continue : bool;
}

(* initial counter state *)
let counter = {
  value = 0;
  step = 0;
  continue = true;
}

(* actions *)
let incr s = s.value <- s.value + 1 ; s.step <- s.step + 1 ; s

let stop s = s.continue <- false ; s

(* property *)
type property = {
  mutable satisfy : bool;
  predicate : state -> bool;
  mutable state : int;
}

let state_predicate s = s.value > 2

(* <>state_predicate *)
let liveness_property = {
  satisfy = false;
  predicate = state_predicate;
  state = 0;
}
