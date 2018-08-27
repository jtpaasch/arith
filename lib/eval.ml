
(** Types of arithmetic expressions. *)
type aexpr =
  | Const of int
  | Plus of aexpr * aexpr

(** Evaluate an expression [e]. *)
let rec eval e =
  match e with
  | Const i -> i
  | Plus (e1, e2) -> eval e1 + eval e2
