type program
type store

(** Parses a given buffer into an AST representation *)
val parse             : Lexing.lexbuf -> program

(** Forward evaluates the given program *)
val feval             : program -> store

(** Backward evaluates the given program *)
val beval             : program -> store

(** Inverts the input program s.t. (2) holds *)
val invert            : program -> program

(** Optimizes the given input program by applying a number of passes *)
val optimize          : program -> program

(** Converts the given ast to a string *)
val string_of_program : program -> string

(** Converts the given global variable store to a string *)
val string_of_store   : store   -> string