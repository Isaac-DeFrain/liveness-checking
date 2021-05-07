(* eventuality property *)
(* <>P means P is eventually satisfied; P is a state predicate *)
type 'a eventuality_property = {
  mutable satisfy : bool;
  predicate : 'a -> bool;
  mutable state : int;
}

(* eventually always property *)
(* <>[]P means P is eventually always satisfied; P is a state predicate *)
(* To verify: check P in every state and [satisfy] <- eval(P), if we end with satisfy = true, then <>[]P is satisfied *)

(* <> state_predicate *)
let eventuality_property pred = {satisfy = false; predicate = pred; state = 0}

(* eventually follows property *)
(* P ~> Q means if P is satisfied, then in a future state, Q must be satisfied
    (false ~> _) = true; P and Q are state predicates *)
(* [conditional] = P and [satisfy] = Q *)
(* Once P has been satisfied and Q becomes satisfied, we no longer change the value of [satisfy] *)
type 'a eventually_follows_property = {
  mutable conditional_satisfy : bool;
  conditional : 'a -> bool;
  mutable satisfy : bool;
  predicate : 'a -> bool;
  mutable state : int;
}

let eventually_follows_property cond pred =
  {
    conditional_satisfy = false;
    conditional = cond;
    satisfy = false;
    predicate = pred;
    state = 0;
  }

(* eventually follows always property *)
(* P ~> []Q means if P is satisfied, then in a future time it becomes the case that Q is always satisfied *)
(* To verify:
    once P is satisfied, [conditional] <- true, and once [conditional] is true,
    check Q in every state and [satisfy] <- eval(Q), if we end with satisfy = true, then P ~> []Q is satisfied  *)
