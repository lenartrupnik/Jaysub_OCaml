type program = Ast.program
type store (* = ??? *)

let parse      = Parser.program Lexer.token (* Do not change *)
let feval      = fun _ -> failwith "implement me!"
let beval      = fun _ -> failwith "implement me!"
let invert     = fun _ -> failwith "implement me!"
let optimize   = fun _ ->
  let constant_fold  = fun _ -> failwith "implement me" in
  let dead_code_elim = fun _ -> failwith "implement me" in
  let proc_inline    = fun _ -> failwith "implement me" in
  failwith "implement me!"

let string_of_program = Ast.string_of_program (* Do not change *)
let string_of_store   = fun _ -> failwith "implement me!"