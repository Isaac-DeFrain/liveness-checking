open State_machine
open Property_types

let action_of_int = function
  | x when x < 3 ->
      Counter.(change incr)
  | x when x >= 3 && x < 6 ->
      Counter.(change decr)
  | 6 ->
      Counter.stop
  | _ ->
      assert false

let print_res (state : Counter.state)
    (prop : Counter.state eventuality_property) =
  if prop.satisfy then
    Printf.printf
      "  Liveness property satisfied:\n\
      \    final counter value: %d\n\
      \    satisfied in state:  %d\n"
      state.value
      prop.state
  else
    Printf.printf
      "  Liveness property violated:\n    final counter value: %d\n"
      state.value

let reset (ctr : Counter.state) (prop : Counter.state eventuality_property) =
  ctr.value <- 0 ;
  ctr.step <- 0 ;
  ctr.continue <- true ;
  prop.satisfy <- false ;
  prop.state <- 0

(* state predicate *)
let state_predicate (s : Counter.state) = s.value > 2

(* eventuality property *)
let diamond pred ctr =
  let liveness_prop = eventuality_property pred in
  let rec loop prop (ctr : Counter.state) =
    if not ctr.continue then (print_res ctr prop ; reset ctr prop)
    else
      let sat = prop.predicate ctr in
      if (not prop.satisfy) && sat then (
        prop.satisfy <- sat ;
        prop.state <- ctr.step ) ;
      let update_state =
        let r =
          Random.(
            set_state @@ State.make_self_init () ;
            int 6)
        in
        if ctr.step > 5 then ctr.continue <- false ;
        if r >= 3 && r < 6 && ctr.value = 0 then fun x -> x
        else action_of_int r
      in
      loop prop (update_state ctr)
  in
  loop liveness_prop ctr

let () = print_string "Eventuality property:\n"

let () = diamond state_predicate Counter.counter

let () = diamond state_predicate Counter.counter

let () = diamond state_predicate Counter.counter

let () = diamond state_predicate Counter.counter

let () = diamond state_predicate Counter.counter

open Phase_counter

(* eventually follows property *)
let change_value_or_phase f =
  if Random.bool () then change_value f else change_phase f

let action_of_int = function
  | x when x < 3 ->
      change_value_or_phase incr
  | x when x >= 3 && x < 6 ->
      fun s ->
        if s.phase mod 2 = 1 then change_value_or_phase decr s
        else change_value_or_phase incr s
  | 6 ->
      stop
  | _ ->
      assert false

let print_res state (prop : state eventually_follows_property) =
  if prop.satisfy then
    Printf.printf
      "  Liveness property satisfied:\n\
      \    final counter value: %d\n\
      \    final phase:         %d\n\
      \    satisfied in state:  %d\n"
      state.value
      state.phase
      prop.state
  else
    Printf.printf
      "  Liveness property violated:\n\
      \    final counter value: %d\n\
      \    final phase:         %d\n"
      state.value
      state.phase

let reset ctr (prop : state eventually_follows_property) =
  ctr.value <- 0 ;
  ctr.phase <- 0 ;
  ctr.step <- 0 ;
  ctr.continue <- true ;
  prop.conditional_satisfy <- false ;
  prop.satisfy <- false ;
  prop.state <- 0

(* state predicates *)
let conditional s = s.value > 1 && s.phase = 0

let satisfy s = s.value > 2 && s.phase = 1

let arrow conditional satisfy ctr =
  let liveness_prop = eventually_follows_property conditional satisfy in
  let rec loop prop ctr =
    let update_state =
      let r =
        Random.(
          set_state @@ State.make_self_init () ;
          int 6)
      in
      if ctr.step > 5 then ctr.continue <- false ;
      action_of_int r
    in
    if not ctr.continue then (print_res ctr prop ; reset ctr prop)
    else if prop.conditional_satisfy then (
      if (not prop.satisfy) && prop.predicate ctr then (
        prop.satisfy <- true ;
        prop.state <- ctr.step ) ;
      loop prop (update_state ctr) )
    else (
      prop.conditional_satisfy <- prop.conditional ctr ;
      loop prop (update_state ctr) )
  in
  loop liveness_prop ctr

let () = print_string "\nEventually follows property:\n"

let () = arrow conditional satisfy counter

let () = arrow conditional satisfy counter

let () = arrow conditional satisfy counter

let () = arrow conditional satisfy counter

let () = arrow conditional satisfy counter
