exception ParseError of string

type stack =
  | S_left_par
  | S_int of int
  | S_plus
