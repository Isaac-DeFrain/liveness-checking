open Counter

let action_of_int = function
  | x when x < 5 ->
      incr
  | 5 ->
      stop
  | _ ->
      assert false

let print_res state prop =
  if prop.satisfy then
    Printf.printf
      "Liveness property satisfied:\n  final counter value %d\n"
      state.value
  else
    Printf.printf
      "Liveness property violated:\n  final counter value %d\n"
      state.value

let reset ctr prop =
  ctr.value <- 0 ;
  ctr.step <- 0 ;
  ctr.continue <- true ;
  prop.satisfy <- false ;
  prop.state <- 0

let rec diamond ctr prop =
  if not ctr.continue then (
    print_res ctr prop ;
    reset counter liveness_property )
  else
    let update_state = action_of_int @@ Random.int 6 in
    let updated_prop =
      if not prop.satisfy then prop.satisfy <- prop.predicate ctr ;
      prop
    in
    diamond (update_state ctr) updated_prop

let () = Random.init @@ Random.int 10

let () = diamond counter liveness_property

let () = diamond counter liveness_property

let () = diamond counter liveness_property

let () = diamond counter liveness_property

let () = diamond counter liveness_property
