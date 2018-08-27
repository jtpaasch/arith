(** Implements {!Cli}. *)

let program_name = "arith"

let expr_value = ref ""
let expr = fun () -> !expr_value

let help_hint = fun () -> Printf.sprintf "See %s --help." program_name

let usage = fun () -> Printf.sprintf "USAGE: arith EXPR"

let spec = []

let handle_args arg =
  match expr () with
  | "" -> expr_value := arg
  | _ ->
    begin
      Printf.printf "Error. Unrecognized argument: '%s'. %s\n%!" arg (help_hint ());
      exit 1
    end

let check () =
  match expr () with
  | "" ->
    begin
      Printf.printf "Error. Please provide an EXPR to parse. %s\n%!" (help_hint ());
      exit 1
    end
  | _ -> ()

let cli () =
  Arg.parse spec handle_args (usage ());
  check ()

