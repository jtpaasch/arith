exception LexerError of string

type t = {
  data : string;
  size : int;
  mutable cursor: int;
}

let init s = { data = s; size = String.length s; cursor = 0 }

let next t = t.cursor <- t.cursor + 1
let next t n = t.cursor <- t.cursor + n

let rec get t pred n =
  if n < t.size && pred t.data.[n]
    then get t pred (n + 1)
    else n

let extract t pred =
  let current_pos = t.cursor in
  let next_pos = get t pred current_pos in
  let diff = next_pos - current_pos in
  t.cursor <- next_pos;
  String.sub t.data current_pos diff

let is_space c =
  match c with
  | ' ' -> true
  | '\t' -> true
  | '\n' -> true
  | _ -> false

let is_left_par c =
  match c with
  | '(' -> true
  | _ -> false

let is_right_par c =
  match c with
  | ')' -> true
  | _ -> false

let is_plus c =
  match c with
  | '+' -> true
  | _ -> false

let is_int c =
  match c with
  | '0'..'9' -> true
  | _ -> false

let extract_space t = extract t is_space
let extract_left_par t = extract t is_left_par
let extract_right_par t = extract t is_right_par
let extract_plus t = extract t is_plus

let extract_int t =
  let str_int = extract t is_int in
  int_of_string str_int

let handle_char c t =
  match c with
  | ' '
  | '\t'
  | '\n' -> Token.T_space (extract_space t)
  | '(' -> Token.T_left_par (extract_left_par t)
  | ')' -> Token.T_right_par (extract_right_par t)
  | '+' -> Token.T_plus (extract_plus t)
  | '0'..'9' -> Token.T_int (extract_int t)
  | _ -> raise (LexerError (Printf.sprintf "Error: can't handle character '%c'" c))

let lex_one t =
  if t.cursor >= t.size
    then Token.T_end
    else handle_char t.data.[t.cursor] t

let rec lex_all t acc =
  let tk = lex_one t in
  match tk with
  | Token.T_end -> acc
  | Token.T_space _ -> lex_all t acc
  | _ -> lex_all t (List.append acc [tk])

let lex s =
  let stream = init s in
  lex_all stream []
