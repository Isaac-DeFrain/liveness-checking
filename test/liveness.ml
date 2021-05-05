open Counter

let action_of_int = function
  | x when x < 3 ->
      change incr
  | x when x >= 3 && x < 6 ->
      change decr
  | 6 ->
      stop
  | _ ->
      assert false

let print_res state (prop : eventuality_property) =
  if prop.satisfy then
    Printf.printf
      "Liveness property satisfied:\n\
      \  final counter value: %d\n\
      \  satisfied in state:  %d\n"
      state.value
      prop.state
  else
    Printf.printf
      "Liveness property violated:\n  final counter value: %d\n"
      state.value

let reset ctr (prop : eventuality_property) =
  ctr.value <- 0 ;
  ctr.step <- 0 ;
  ctr.continue <- true ;
  prop.satisfy <- false ;
  prop.state <- 0

(* state predicate *)
let state_predicate s = s.value > 2

(* TODO somewhow the prop.state always returns 6... *)
let diamond pred ctr =
  let liveness_prop = liveness_property pred in
  let rec loop prop ctr =
    if not ctr.continue then (print_res ctr prop ; reset counter prop)
    else
      let sat = prop.predicate ctr in
      if (not prop.satisfy) && sat then (
        prop.satisfy <- sat ;
        prop.state <- ctr.step ) ;
      let update_state =
        let r = Random.int 6 in
        if ctr.step > 5 then ctr.continue <- false ;
        if r >= 3 && r < 6 && ctr.value = 0 then fun x -> x
        else action_of_int r
      in
      loop prop (update_state ctr)
  in
  loop liveness_prop ctr

let () = Random.init @@ Random.int 100

let () = diamond state_predicate counter

let () = diamond state_predicate counter

let () = diamond state_predicate counter

let () = diamond state_predicate counter

let () = diamond state_predicate counter
