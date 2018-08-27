
type t =
  | T_end
  | T_space of string
  | T_left_par of string
  | T_right_par of string
  | T_plus of string
  | T_int of int

let to_string t =
  match t with
  | T_end -> "EOF"
  | T_space _ -> "[space]"
  | T_left_par _ -> "( "
  | T_right_par _ -> " )"
  | T_plus _ -> " + "
  | T_int n -> Printf.sprintf "%d" n

