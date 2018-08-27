(** Manages/defines the CLI for the program. *)

(** [expr ()] retrieves the value of the EXPR argument. *) 
val expr : unit -> string

(** [cli ()] parses the command line arguments. *)
val cli : unit -> unit

