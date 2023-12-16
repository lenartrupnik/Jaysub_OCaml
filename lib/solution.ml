
module B = Base
type program = Ast.program
(**Type representing a store which is basically a list of pair of AST identifiers (string) and their integer values*)
type store = (Ast.identifier * int) list
let parse      = Parser.program Lexer.token (* Do not change *)

(** Updates the store with a new value for a given identifier.
  @param id Identifier that need to be updated.
  @param value New value for the identifier.
  @param store Store that needs to be updated.
  @return Updated store with new value of identifier.
*)
let update_store (id : Ast.identifier) (value : int) (store : store) : store =
  (id, value) :: List.remove_assoc id store

(** Converts a store to a string representation.
  @param id Identifier to be updated.
  @return String representation of the store.
*)
let string_of_store (store : store) : string =
  let pair_to_string (id, value) = id ^ ": " ^ string_of_int value in
  String.concat ", " (List.map pair_to_string store)

(** Finds a procedure by its identifier from a list of procedures.
  @param id Identifier of the procedure to find.
  @param List of procedures to search from.
  @return Option type containing the list of statements of the procedure if found, None otherwise.
*)
let find_procedure_by_id (id : Ast.identifier) (procedures : Ast.procedure list) : Ast.statement list option =
  let rec find_procedure_by_id_helper id = function
  | [] -> None
  | (proc_id, stmts) :: rest ->
    if proc_id = id then
      Some stmts
    else
      find_procedure_by_id_helper id rest
  in
  find_procedure_by_id_helper id procedures

(** Evaluates a binary operation.
  @param op Binary operation to be evaluated.
  @param v1 First operand.
  @param v2 Second operand.
  @param forward Direction of evaluation (true for forward, false for backward).
  @return Result of the binary operation.
*)
let eval_binop (op : Ast.binop) (v1 : int) (v2 : int) (forward : bool) : int =
  match op with
  | Ast.Plus -> if forward then v1 + v2 else v1 - v2
  | Ast.Min -> if forward then v1 - v2 else v1 + v2
  | Ast.Mult -> 
    if forward then 
      v1 * v2 
    else 
      if v2 = 0 then failwith "Division by zero..." else v1 / v2
  | Ast.Div -> 
    if forward then 
      if v2 = 0 then failwith "Division by zero" else v1 / v2 
    else v1 * v2
  | Ast.Eq -> if v1 = v2 then 1 else 0
  | Ast.NEq -> if v1 <> v2 then 1 else 0

(** Evaluates an expression with new regards to given store.
  @param expr Expression to be evaluated.
  @param store Store used for the evaluation context.
  @param forward Direction of evaluation (true for forward, false for backward).
  @return Integer result of the evaluation.
*)
let eval_expr (expr : Ast.expression) (store : store) (forward : bool) : int =
  let rec eval (e : Ast.expression) (s : store) (fwd : bool) : int =
    match e with
    | Ast.Constant c -> c
    | Ast.Ident id -> List.assoc id s
    | Ast.Binop (e1, op, e2) ->
      let v1 = eval e1 s fwd in
      let v2 = eval e2 s fwd in
      let result = eval_binop op v1 v2 fwd in
      result
  in
  eval expr store forward
    
(** [feval program] forward evaluates the given [program]. It executes each procedure
  in the program in a forward direction and produces a [store] containing the final state
  of the program variables.
  @param program The program to be evaluated.
  @return The final state of the program as a [store]. *)
let rec feval(program: program) : store =
    match program with
    | _, [] -> failwith "No procedures found in the program"
    | _, procedures ->
      let initial_store = List.map (fun id -> (id, 0)) (fst program) in
      execute_procedure (List.hd (List.rev procedures)) initial_store procedures true

    (** Function that executes a given [procedure]
        either in a forward or backward direction based on the [forward] parameter. 
        It utilizes [execute_statements] to process each statement in the procedure.
        @param procedure The procedure to be executed.
        @param store The current state of the program variables.
        @param all_procedures The list of all procedures in the program, for procedure calls.
        @param forward Boolean flag to indicate the direction of execution.
        @return The updated state of the program variables as a [store]. *)
    and execute_procedure (procedure: Ast.procedure) (store : store) (all_procedures : Ast.procedure list) (forward : bool) : store =
      let _, statements = procedure in
      (*Printf.printf "Executing procedure %s\n" proc_id;*)
      if forward then
        execute_statements statements store all_procedures true
      else
        execute_statements (List.rev statements) store all_procedures false
    
    (** Function that maps the execution of given [statements]
        either in a forward or backward direction based on the [forward] parameter. 
        It utilizes [execute_statements] to process each statement in the procedure.
        @param statements The statements to be maped.
        @param store The current state of the program variables.
        @param all_procedures The list of all procedures in the program, for procedure calls.
        @param forward Boolean flag to indicate the direction of execution.
        @return The updated state of the program variables as a [store]. *)
    and execute_statements (statements : Ast.statement list) (store: store) (all_procedures : Ast.procedure list) (forward : bool) : store =
      match statements with
      | [] -> store
      | statement :: rest ->
        if forward then
          let new_store = execute_statement statement store all_procedures true in
          execute_statements rest new_store all_procedures true
        else
          let new_store = execute_statement statement store all_procedures false in
          execute_statements rest new_store all_procedures false
    
    (** Function that execute given [statement]
        either in a forward or backward direction based on the [forward] parameter. 
        @param statement The statement to be executed.
        @param store The current state of the program variables.
        @param all_procedures The list of all procedures in the program, for procedure calls.
        @param forward Boolean flag to indicate the direction of execution.
        @return The updated state of the program variables as a [store]. *)
    and execute_statement (statement : Ast.statement) (store : store) (all_procedures : Ast.procedure list) (forward : bool) : store =
      match statement with
      | Ast.ModStmt (id, modi, expr) ->
        let current_value = List.assoc id store in
        let result = eval_expr expr store true in
        let update_value = eval_binop modi current_value result forward in
        update_store id update_value store
    
      | Ast.SwapStmt (id1, id2) ->
        let value1 = List.assoc id1 store in
        let value2 = List.assoc id2 store in
        let updated_store = update_store id1 value2 (update_store id2 value1 store) in
        updated_store

      | Ast.IfStmt (cond, then_stmts, else_stmts, assertion) ->
        if (forward && eval_expr cond store forward <> 0) || (not forward && eval_expr assertion store forward <> 0) then
          begin
            let updated_store = if forward then execute_statements then_stmts store all_procedures true
                                else execute_statements (List.rev then_stmts) store all_procedures false in
            if (forward && eval_expr assertion updated_store forward<> 0) || (not forward && eval_expr cond updated_store forward <> 0) then
              updated_store
            else
              failwith "Assertion failed after executing then-branch"
          end
        else
          begin
            let updated_store = if forward then execute_statements else_stmts store all_procedures true
                                else execute_statements (List.rev else_stmts) store all_procedures false in
            if (forward && eval_expr assertion updated_store forward<> 0) || (not forward && eval_expr cond updated_store forward <> 0) then
              failwith "Assertion failed after executing else-branch"
            else
              updated_store
          end

      | Ast.CallStmt procedure_name ->
        execute_procedure_by_id procedure_name store all_procedures forward
        
      | Ast.UncallStmt procedure_name ->
        execute_procedure_by_id procedure_name store all_procedures (not forward)      
      
      | Ast.LoopStmt (assertion, loop_body, test) ->
        execute_loop assertion loop_body test store all_procedures forward
      | _ -> failwith ("Unsupported statement type: " ^ Ast.show_statement statement)
    
    and execute_procedure_by_id (procedure_name : Ast.identifier) (store : store) (all_procedures : Ast.procedure list) (forward : bool) : store =
        match find_procedure_by_id procedure_name all_procedures with
        | Some stmts ->
          let statements = if forward then stmts else List.rev stmts in
          execute_statements statements store all_procedures forward
        | None -> failwith ("Procedure not found: " ^ procedure_name)

    and execute_loop (assertion : Ast.expression) (loop_body : Ast.statement list) (test : Ast.expression) (store : store) (all_procedures : Ast.procedure list) (forward : bool) : store =
      if (forward && eval_expr assertion store forward<> 0) || (not forward && eval_expr test store forward <> 0) then
        let rec loop (store : store) : store =
            let updated_store = if forward then execute_statements loop_body store all_procedures true
                            else execute_statements (List.rev loop_body) store all_procedures false in
            if (forward && eval_expr test updated_store forward = 0) || (not forward && eval_expr assertion updated_store forward = 0) then
              loop updated_store
            else
              updated_store
          in
        loop store
      else
        failwith "Loop assertion failed initially"
(*-----------------------------------------------------*)

(** Function that backward evaluates the given [program]. It is similar to [feval] but
  executes the program in the reverse direction.
  @param program The program to be backward evaluated.
  @return The final state of the program as a [store]. *)
let beval(program: program) : store =
  match program with
  | _, [] -> failwith "No procedures found in the program"
  | _, procedures ->
    let initial_store = List.map (fun id -> (id, 0)) (fst program) in
    execute_procedure (List.hd (List.rev procedures)) initial_store procedures false
          
(*-----------------------------------------------------*)

(** [invert program] takes a [program] and produces its inverted version. The inverted program,
  when executed with updated_store from feval of initial program, should ideally reverse the 
  effects of the original program.
  @param program The program to be inverted.
  @return The inverted program. *)
let invert (program: program) : program =
  let rec invert_stmt (stmt: Ast.statement) : Ast.statement =
    match stmt with
    | Ast.ModStmt (id, modi, expr) ->
      let inverted_modi = match modi with
        | Ast.Plus -> Ast.Min
        | Ast.Min -> Ast.Plus
        | Ast.Mult -> Ast.Div
        | Ast.Div -> Ast.Mult
        | _ -> failwith "Unsupported modification in ModStmt for inversion"
      in
      Ast.ModStmt (id, inverted_modi, expr)

    | Ast.SwapStmt (id1, id2) ->
      Ast.SwapStmt (id1, id2)

    | Ast.IfStmt (cond, then_stmts, else_stmts, assertion) ->
      let inverted_then_stmts = List.rev_map invert_stmt then_stmts in
      let inverted_else_stmts = List.rev_map invert_stmt else_stmts in
      Ast.IfStmt (assertion, inverted_then_stmts, inverted_else_stmts, cond)

    | Ast.CallStmt proc_name ->
      Ast.CallStmt proc_name

    | Ast.UncallStmt proc_name ->
      Ast.CallStmt proc_name

    | Ast.LoopStmt (assertion, loop_body, test) ->
      let inverted_loop_body = List.rev_map invert_stmt loop_body in
      Ast.LoopStmt (test, inverted_loop_body, assertion)

    | _ -> failwith "Unsupported statement type for inversion"
  in

  let invert_proc (proc: Ast.procedure) : Ast.procedure =
    let proc_id, stmts = proc in
    (proc_id, List.rev_map invert_stmt stmts)
  in

  let vars, procs = program in
  let string_of_program = Ast.string_of_program (* Do not change *) in
  let inv_program = (vars, List.map invert_proc procs) in
  inv_program

(*-----------------------------------------------------*)
let optimize   = fun _ ->
  let constant_fold  = fun _ -> failwith "implement me" in
  let dead_code_elim = fun _ -> failwith "implement me" in
  let proc_inline    = fun _ -> failwith "implement me" in
  failwith "implement me!"

  let string_of_program = Ast.string_of_program (* Do not change *)

(*
----------------------------------------------------
------------------ Unit Tests ----------------------
----------------------------------------------------

*)

let%test "update_store functionality test" =
let initial_store = [("x", 10); ("y", 20)] in
let updated_store = update_store "x" 15 initial_store in
B.List.Assoc.find updated_store ~equal:String.equal "x" = Some 15

let%test "feval basic functionality" =
  let program = (["x"; "y"], [("main", [Ast.ModStmt ("x", Ast.Plus, Ast.Constant 5)])]) in
  let store = feval program in
  B.List.Assoc.find store ~equal:String.equal "x" = Some 5

let%test "feval loop functionality" =
  let program = (["x"], [("main", [Ast.LoopStmt (Ast.Constant 1, [Ast.ModStmt ("x", Ast.Plus, Ast.Constant 1)], Ast.Binop (Ast.Ident "x", Ast.Eq, Ast.Constant 5))])]) in
  let store = feval program in
  match List.find_opt (fun (id, _) -> id = "x") store with
  | Some (_, value) -> value = 5
  | None -> false

let%test "feval if statement functionality" =
  let program = (["x"; "y"], [("main", [Ast.IfStmt (Ast.Binop (Ast.Constant 1, Ast.Eq, Ast.Constant 1), [Ast.ModStmt ("x", Ast.Plus, Ast.Constant 10)], [Ast.ModStmt ("y", Ast.Plus, Ast.Constant 10)], Ast.Constant 10)])]) in
  let store = feval program in
  match List.find_opt (fun (id, _) -> id = "x") store with
  | Some (_, value) -> value = 10
  | None -> false

let%test "feval procedure call functionality" =
  let program = (["x"], [("increment", [Ast.ModStmt ("x", Ast.Plus, Ast.Constant 1)]); ("main", [Ast.CallStmt "increment"])]) in
  let store = feval program in
  match List.find_opt (fun (id, _) -> id = "x") store with
  | Some (_, value) -> value = 1
  | None -> false

let%test "feval additional expression" =
  let program = (["x"; "y"], [("main", [Ast.ModStmt ("x", Ast.Plus, Ast.Binop (Ast.Constant 2, Ast.Mult, Ast.Constant 3)); Ast.ModStmt ("y", Ast.Plus, Ast.Binop (Ast.Ident "x", Ast.Div, Ast.Constant 2))])]) in
  let store = feval program in
  match List.find_opt (fun (id, _) -> id = "y") store with
  | Some (_, value) -> value = 3
  | None -> false

let%test "feval uncall functionality" =
  let program = 
    (["x"], 
      [ ("increment", [Ast.ModStmt ("x", Ast.Plus, Ast.Constant 1)]);
        ("main", [Ast.CallStmt "increment"; Ast.UncallStmt "increment"]) ]) in
  let final_store = feval program in
  match List.find_opt (fun (id, _) -> id = "x") final_store with
  | Some (_, value) -> value = 0
  | None -> false

  let%test "finding procedure in find_procedure_by_id" =
  let procedures = [("proc1", [Ast.ModStmt ("x", Ast.Plus, Ast.Constant 5)]); ("proc2", [Ast.SwapStmt ("x", "y")])] in
  match find_procedure_by_id "proc1" procedures with
  | Some stmts -> 
    (match List.hd stmts with
     | Ast.ModStmt (id, _, _) -> String.equal id "x"
     | _ -> false)
  | None -> false

let%test "find_procedure_by_id with non-existing procedure" =
  let procedures = [("proc1", [Ast.ModStmt ("x", Ast.Plus, Ast.Constant 5)]); ("proc2", [Ast.SwapStmt ("x", "y")])] in
  find_procedure_by_id "proc3" procedures = None

let%test "beval reverses a complex program" =
  let fwr_program = 
    (["x"; "y"; "i"], 
     [ ("increment_x", [Ast.ModStmt ("x", Ast.Plus, Ast.Constant 2)]);
       ("decrement_y", [Ast.ModStmt ("y", Ast.Min, Ast.Constant 1)]);
       ("main", [Ast.CallStmt "increment_x";
                Ast.ModStmt ("i", Ast.Plus, Ast.Constant 3);
                Ast.LoopStmt (Ast.Binop (Ast.Ident "i", Ast.NEq, Ast.Constant 0),
                             [Ast.CallStmt "decrement_y"; Ast.CallStmt "increment_x"; Ast.ModStmt ("i", Ast.Min, Ast.Constant 1)], 
                             Ast.Binop (Ast.Ident "i", Ast.Eq, Ast.Constant 0))]) ]) in

  let fwr_updated_store_program = 
    (["x"; "y"; "i"], 
    [ ("increment_x", [Ast.ModStmt ("x", Ast.Plus, Ast.Constant 2)]);
      ("decrement_y", [Ast.ModStmt ("y", Ast.Min, Ast.Constant 1)]);
      ("unmain", [Ast.LoopStmt (Ast.Binop (Ast.Ident "i", Ast.Eq, Ast.Constant 0),
                   [Ast.ModStmt ("i", Ast.Plus, Ast.Constant 1); Ast.UncallStmt "increment_x"; Ast.UncallStmt "decrement_y"], 
                   Ast.Binop (Ast.Ident "i", Ast.NEq, Ast.Constant 0));
                 Ast.ModStmt ("i", Ast.Min, Ast.Constant 3);
                 Ast.UncallStmt "increment_x"]) ]) in

  let updated_store = feval fwr_program in
  let beval_store = beval fwr_updated_store_program in
  updated_store = beval_store

let%test "invert program and forward on updated store to get back initial store" =
  let initial_program = 
    (["x"; "y"], 
      [ ("increment_x", [Ast.ModStmt ("x", Ast.Plus, Ast.Constant 1)]);
        ("increment_y", [Ast.ModStmt ("y", Ast.Plus, Ast.Constant 2)]);
        ("main", [Ast.CallStmt "increment_x"; Ast.CallStmt "increment_x"; Ast.CallStmt "increment_y"]) ]) in
  let current_path = Sys.getcwd () in
  let initial_store = [("x", 0); ("y", 0)] in
  let updated_store = feval initial_program in
  let inverted_program = invert initial_program in
  let initialization_stmts = List.map (fun (id, value) -> Ast.ModStmt (id, Ast.Plus, Ast.Constant value)) updated_store in
  let modified_inverted_program = match inverted_program with
    | (vars, procs) ->
      let modified_main_proc = 
        List.map (fun (proc_name, stmts) -> 
          if proc_name = "main" then 
            (proc_name, initialization_stmts @ stmts) 
          else 
            (proc_name, stmts)
        ) procs in
      (vars, modified_main_proc) in
  let final_store = feval modified_inverted_program in
  final_store = initial_store
  
let read_file filename =
  In_channel.with_open_bin filename In_channel.input_all

let%test "test feval on fib.jsub" =
  
  let program_str = read_file "../../../../../examples/fib.jsub" in
  let lexbuf = Lexing.from_string program_str in
    let program = parse lexbuf in
    let final_store = feval program in
    (* Add your test assertions here *)
    B.List.Assoc.find final_store ~equal:String.equal "x1" = Some 5 &&
    B.List.Assoc.find final_store ~equal:String.equal "x2" = Some 8 &&
    B.List.Assoc.find final_store ~equal:String.equal "n" = Some 0

let%test "test feval on doublebit.jsub" =
  let program_str = read_file "../../../../../examples/doublebit.jsub" in
  let lexbuf = Lexing.from_string program_str in
  let program = parse lexbuf in
  let final_store = feval program in
  (* Add your test assertions here *)
  B.List.Assoc.find final_store ~equal:String.equal "z" = Some 0 &&
  B.List.Assoc.find final_store ~equal:String.equal "bit" = Some 0 &&
  B.List.Assoc.find final_store ~equal:String.equal "n" = Some 4

let%test "test feval on occur-check.jsub" =
  let program_str = read_file "../../../../../examples/occurs-check.jsub" in
  let lexbuf = Lexing.from_string program_str in
    let program = parse lexbuf in
    let final_store = feval program in
    B.List.Assoc.find final_store ~equal:String.equal "x" = Some 0

(**Helper function to make testing more compact
    @parameter file_path*)
let test_invert_with_feval_and_beval file_path =
  let lexbuf = Lexing.from_string (read_file file_path) in
  let program = parse lexbuf in
  let updated_store = feval program in
  let inverted_program = invert program in
  (* Prepare the inverted program with intitial statements corresponding to updated store *)
  let initialization_stmts = List.map (fun (id, value) -> Ast.ModStmt (id, Ast.Plus, Ast.Constant value)) updated_store in
  let modified_inverted_program =
    let modify_procedure (proc_name, stmts) =
      if proc_name = "main" then
        (proc_name, initialization_stmts @ stmts)
      else
        (proc_name, stmts)
    in
    match inverted_program with
    | (vars, procs) -> (vars, List.map modify_procedure procs)
  in
  let final_store = feval modified_inverted_program in
  let beval_store = beval inverted_program in
  (* Check if all if holds: initial_store -> feval(program) -> feval(invert program) -> initial_store*)
  (* Check if all if holds: store -> feval(program) = beval(inver program)*)
  let all_variables_zero store = List.for_all (fun (_, value) -> value = 0) store in
  all_variables_zero final_store && beval_store = updated_store

let%test "invert and evaluate switch program" =
  test_invert_with_feval_and_beval "../../../../../examples/swtch.jsub"

let%test "invert and evaluate fib program" =
  test_invert_with_feval_and_beval "../../../../../examples/fib.jsub"
