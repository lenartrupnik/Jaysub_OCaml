include Utils

module S = Jaysub.Solution

type task = FEval | BEval | Invert | Optimize

(** Executes a task, returning either a string of result store or of the program *)
let execute_task t = S.(match t with
  | FEval    -> string_of_store   << feval
  | BEval    -> string_of_store   << beval
  | Invert   -> string_of_program << invert
  | Optimize -> string_of_program << optimize)

let parse_args () =
  if Array.length Sys.argv <> 3 then
    failwith "Usage: jaysub [forward|backward|invert|optimize] <input.jsub>"
  else
  Sys.argv.(2), match Sys.argv.(1) with
      | "forward"  -> FEval
      | "backward" -> BEval
      | "invert"   -> Invert
      | "optimize" -> Optimize
      | _          -> failwith "Invalid mode. Use 'forward' or 'backward' or 'invert' or 'optimize'."

let () =
  let (input, t) = parse_args () in
  let contents = In_channel.with_open_bin input In_channel.input_all in
  let lexbuf = Lexing.from_string contents in
  try S.(parse lexbuf |> execute_task t |> print_string)
  with
  | Jaysub.Lexer.Error msg ->
      Printf.fprintf stderr "%a: %s\n%!" print_position lexbuf msg
  | Jaysub.Parser.Error ->
      Printf.fprintf stderr "Could not parse input at %s%a. Syntax error.\n%!" input print_position lexbuf; exit (-1)
