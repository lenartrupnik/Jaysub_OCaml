
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
  @param forward Direction of evaluation.
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
            if (forward && eval_expr assertion updated_store forward <> 0) || (not forward && eval_expr cond updated_store forward <> 0) then
              updated_store
            else
              failwith "Assertion failed after executing then-branch"
          end
        else
          begin
            let updated_store = if forward then execute_statements else_stmts store all_procedures true
                                else execute_statements (List.rev else_stmts) store all_procedures false in
            if (forward && eval_expr assertion updated_store forward <> 0) || (not forward && eval_expr cond updated_store forward <> 0) then
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

      | Ast.SkipStmt -> store

    and execute_procedure_by_id (procedure_name : Ast.identifier) (store : store) (all_procedures : Ast.procedure list) (forward : bool) : store =
        match find_procedure_by_id procedure_name all_procedures with
        | Some stmts ->
          let statements = if forward then stmts else List.rev stmts in
          execute_statements statements store all_procedures forward
        | None -> failwith ("Procedure not found: " ^ procedure_name)

    and execute_loop (assertion : Ast.expression) (loop_body : Ast.statement list) (test : Ast.expression) (store : store) (all_procedures : Ast.procedure list) (forward : bool) : store =
        let rec loop (store : store) (first_pass: bool): store =
          if first_pass then
            if (forward && eval_expr assertion store forward <> 0) || (not forward && eval_expr test store forward <> 0) then
              let updated_store = if forward then execute_statements loop_body store all_procedures true
                                  else execute_statements (List.rev loop_body) store all_procedures false in
              if (forward && eval_expr test updated_store forward = 0) || (not forward && eval_expr assertion updated_store forward = 0) then
                loop updated_store false
              else
                updated_store
            else 
              store
          else
            if (forward && eval_expr assertion store forward == 0) || (not forward && eval_expr test store forward == 0) then
              let updated_store = if forward then execute_statements loop_body store all_procedures true
                              else execute_statements (List.rev loop_body) store all_procedures false in
              if (forward && eval_expr test updated_store forward = 0) || (not forward && eval_expr assertion updated_store forward = 0) then
                loop updated_store false
              else
                updated_store
            else
              failwith "Assertion should only hold for the first iteration"
          in
        loop store true
(*-----------------------------------------------------*)
(*--------------------BEVAL PROGRAM--------------------*)
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
(*-------------------INVERT PROGRAM--------------------*)
(*-----------------------------------------------------*)

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

let invert_proc (proc: Ast.procedure) : Ast.procedure =
  let proc_id, stmts = proc in
  (proc_id, List.rev_map invert_stmt stmts)

(** [invert program] takes a [program] and produces its inverted version. The inverted program,
  when executed with updated_store from feval of initial program, should ideally reverse the 
  effects of the original program.
  @param program The program to be inverted.
  @return The inverted program. *)

let invert (program: program) : program =
  let vars, procs = program in
  let inv_program = (vars, List.map invert_proc procs) in
  inv_program

(*-----------------------------------------------------*)
(*-----------------PROGRAM OPTIMIZATION----------------*)
(*-----------------------------------------------------*)

(** [constant_fold_expr] optimizes an expression by applying constant folding. 
    This includes simplifying expressions with constant operands and applying algebraic simplifications.
    @param expr The expression to be optimized.
    @return The optimized expression, with constant expressions folded where possible. *)
let rec constant_fold_expr (expr: Ast.expression) : Ast.expression =
  match expr with
  | Ast.Binop (Ast.Constant c1, op, Ast.Constant c2) ->
    Ast.Constant (eval_binop op c1 c2 true)

  | Ast.Binop (e1, op, e2) ->
    let e1' = constant_fold_expr e1 in
    let e2' = constant_fold_expr e2 in
    begin match (e1', e2', op) with
      | (Ast.Constant c1', Ast.Constant c2', _) ->
          Ast.Constant (eval_binop op c1' c2' true)
      (* Here is a little weird part where I try to implement (not the best) mathematical associative & commutative laws
         Definitily could use some improvements*)
      | (Ast.Binop (left, Ast.Plus, Ast.Constant c1'), Ast.Constant c2', Ast.Plus) 
      | (Ast.Constant c1', Ast.Binop (Ast.Constant c2', Ast.Plus, left), Ast.Plus) ->
          constant_fold_expr (Ast.Binop (left, Ast.Plus, Ast.Constant (c1' + c2')))
      | (Ast.Binop (left, Ast.Min, Ast.Constant c1'), Ast.Constant c2', Ast.Min) 
      | (Ast.Constant c1', Ast.Binop (Ast.Constant c2', Ast.Min, left), Ast.Min) ->
          constant_fold_expr (Ast.Binop (left, Ast.Min, Ast.Constant (c1' - c2')))
      | (Ast.Binop (left, Ast.Mult, Ast.Constant c1'), Ast.Constant c2', Ast.Mult) 
      | (Ast.Constant c1', Ast.Binop (Ast.Constant c2', Ast.Mult, left), Ast.Mult) ->
          constant_fold_expr (Ast.Binop (left, Ast.Mult, Ast.Constant (c1' * c2')))
      | (Ast.Binop (left, Ast.Div, Ast.Constant c1'), Ast.Constant c2', Ast.Div) when c2' <> 0 ->
          constant_fold_expr (Ast.Binop (left, Ast.Div, Ast.Constant (c1' / c2')))
      | (_, _, Ast.Min) when e1' = e2' ->
          Ast.Constant 0
      | (_, _, Ast.Div) when e1' = e2' && e1' <> Ast.Constant 0 ->
          Ast.Constant 1
      | (_, _, Ast.Mult) when e1' = Ast.Constant 0 || e2' = Ast.Constant 0 ->
          Ast.Constant 0
      | _ ->
          Ast.Binop (e1', op, e2')
    end
  | _ -> expr

(** [constant_fold_stmt] applies constant folding optimization to a statement. 
    For statements containing expressions, it applies [constant_fold_expr] to optimize those expressions.
    @param stmt The statement to be optimized.
    @return The optimized statement with constant expressions folded, if applicable. *)

let rec constant_fold_stmt (stmt: Ast.statement) : Ast.statement =
  match stmt with
  | Ast.ModStmt (id, modi, expr) ->
    let expr' = constant_fold_expr expr in
    Ast.ModStmt (id, modi, expr')
  | Ast.IfStmt (cond, then_stmts, else_stmts, assertion) ->
    let cond' = constant_fold_expr cond in
    let assertion' = constant_fold_expr assertion in
    Ast.IfStmt (cond', List.map constant_fold_stmt then_stmts, List.map constant_fold_stmt else_stmts, assertion')
  | Ast.LoopStmt (assertion, loop_body, test) ->
    let assertion' = constant_fold_expr assertion in
    let test' = constant_fold_expr test in
    Ast.LoopStmt (assertion', List.map constant_fold_stmt loop_body, test')
  | SwapStmt (id1, id2) -> SwapStmt (id1, id2)
  | CallStmt proc_name -> CallStmt proc_name
  | UncallStmt proc_name -> UncallStmt proc_name
  | SkipStmt -> SkipStmt

(** [dead_code_elim_stmt] applies dead code elimination to a given statement. 
    It recursively processes conditional and loop structures to remove code that 
    will never be executed based on static analysis of constant expressions.

    @param stmt The statement to be processed for dead code elimination.
    @return A list of statements after applying dead code elimination. The list 
            may be empty if the entire statement is identified as dead code.

    Note: The function also raises an exception if it encounters a loop with a condition 
    that is statically determined to be always true, indicating a potential infinite loop. The same
    for test condition being always false. *)

let rec dead_code_elim_stmt (stmt: Ast.statement) : Ast.statement list =
  match stmt with
  | Ast.IfStmt (Ast.Constant 0, _, else_stmts, _) ->
    List.concat (List.map dead_code_elim_stmt else_stmts)
  | Ast.IfStmt (Ast.Constant _, then_stmts, _, _) ->
    List.concat (List.map dead_code_elim_stmt then_stmts)
  
  | Ast.IfStmt (_, _, _, Ast.Constant 0) ->
    []
  | Ast.IfStmt (cond, then_stmts, else_stmts, assertion) ->
    [Ast.IfStmt (cond, 
                List.concat (List.map dead_code_elim_stmt then_stmts), 
                List.concat (List.map dead_code_elim_stmt else_stmts), 
                assertion)]
  | Ast.LoopStmt (Ast.Constant 0, _, _) ->
    []
  | Ast.LoopStmt (_, _, Ast.Constant 0) ->
    failwith "Error: Loop test is always false, potentially creating an infinite loop"
  | Ast.LoopStmt (Ast.Constant value, _, _) when value <> 0 ->
    failwith "Error: Loop condition is always true, potentially creating an infinite loop"

  | Ast.LoopStmt (assertion, loop_body, test) ->
    [Ast.LoopStmt (assertion, 
                    List.concat (List.map dead_code_elim_stmt loop_body), 
                    test)]
  | _ -> 
    [stmt]

(** [remove_skip_stmts] filters out 'SkipStmt' from a list of statements. 
    @param stmts List of statements possibly containing 'SkipStmt'.
    @return A list of statements with 'SkipStmt' removed. *)
let remove_skip_stmts stmts =
  List.filter (function | Ast.SkipStmt -> false | _ -> true) stmts

module StringSet = Set.Make(String) 
(** [remove_uncalled_procedures] Helper function for inline optimization. Function that filters out 
    procedures from a program that are not called anywhere in the last procedure's body. It ensures 
    that the last procedure is always retained, assuming it is the main entry point of the program. 
    Procedures are considered 'uncalled' if they are not referenced by any CallStmt in the last 
    procedure.

    @param program The program containing variables and a list of procedures.
    @return The program with the same variables but potentially fewer procedures.*)

let remove_uncalled_procedures (program: Ast.program) : Ast.program =
  let vars, procs = program in
  let extract_calls stmts calls =
    List.fold_left (fun acc stmt ->
      match stmt with
      | Ast.CallStmt name -> StringSet.add name acc
      | _ -> acc
    ) calls stmts
  in

  let last_proc =
    match List.rev procs with
    | last_proc :: _ -> last_proc
    | [] -> failwith "No procedures found in the program"
  in
  let last_proc_name, last_proc_stmts = last_proc in
  let called_procs = extract_calls last_proc_stmts StringSet.empty in

  let filtered_procs =
    List.filter (fun (name, _) ->
      StringSet.mem name called_procs || name = last_proc_name
    ) procs
  in

  (vars, filtered_procs)
  

(** [inline_procedures] replaces procedure calls within a program with the actual body of the procedures.
    It handles both regular procedure calls and uncalls (reverse calls). For uncalls, it inverts the 
    procedure body before inlining. The function ensures that after inlining, only the last procedure 
    in the program is retained, assuming it to be the main entry point of the program.

    @param program The input program consisting of variables and a list of procedures.
    @return A modified program where procedure calls are replaced with the inlined procedure bodies.*)
let inline_procedures (program: Ast.program) : Ast.program =
  let vars, procs = program in

  let rec inline_stmt stmt =
    match stmt with
    | Ast.CallStmt proc_name | Ast.UncallStmt proc_name ->
      let is_uncall = match stmt with
        | Ast.UncallStmt _ -> true
        | _ -> false
      in
      (match find_procedure_by_id proc_name procs with
      | Some body ->
          let body_to_inline = 
            if is_uncall then
              let _, inverted_body = invert_proc (proc_name, body) in
              inverted_body
            else body
          in
          List.concat_map inline_stmt body_to_inline
      | None -> failwith ("Procedure not found: " ^ proc_name))
    | _ -> [stmt]
  in
  let inline_proc (name, body) =
    (name, List.concat_map inline_stmt body)
  in

  let inlined_procs = List.map inline_proc procs in
  let last_proc = match List.rev inlined_procs with
    | [] -> failwith "No procedures found in the program"
    | last :: _ -> [last]
  in
  (vars, last_proc)

(** [optimize] performs a series of optimizations on a given program. It consists of three main steps:
    Constant Folding (Simplifying constant expressions), Procedure Inlining (Replacing procedure calls 
    with actual body of call) and Dead Code Elimnation (Removing code that will never be reached)

  @param program The program to be optimized.
  @return An optimized program where constant expressions are simplified, uncalled procedures (except the last one)
          are removed, and procedure calls are inlined.*)


let optimize (program: Ast.program) : Ast.program =
  let constant_fold (program: Ast.program) : Ast.program =
    let fold_stmts stmts = List.map constant_fold_stmt stmts in
    let fold_proc (name, stmts) = (name, fold_stmts stmts) in
    let vars, procs = program in
    (vars, List.map fold_proc procs) 
  in

  let proc_inline (program: Ast.program) : Ast.program =
    inline_procedures program
    |> remove_uncalled_procedures
  in

  let dead_code_elim (program: Ast.program) : Ast.program =
    let vars, procs = program in
  
    let eliminate_and_clean_stmts stmts =
      stmts
      |> List.concat_map dead_code_elim_stmt
      |> remove_skip_stmts
    in
    let cleaned_procs = List.map (fun (name, stmts) -> (name, eliminate_and_clean_stmts stmts)) procs in
  
    (vars, cleaned_procs)
  in
  program
  |> constant_fold
  |> dead_code_elim
  |> proc_inline


let string_of_program = Ast.string_of_program (* Do not change *)

(*
----------------------------------------------------
-----------------------TESTING----------------------
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

let%test "feval loop functionality V2" =
  let program = (["x"], [("main", [Ast.LoopStmt (Ast.Binop(Ast.Ident "x", Ast.Eq, Ast.Constant 0), [Ast.ModStmt ("x", Ast.Plus, Ast.Constant 1)], Ast.Binop (Ast.Ident "x", Ast.Eq, Ast.Constant 5))])]) in
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
                Ast.LoopStmt (Ast.Binop (Ast.Ident "i", Ast.Eq, Ast.Constant 3),
                             [Ast.CallStmt "decrement_y"; Ast.CallStmt "increment_x"; Ast.ModStmt ("i", Ast.Min, Ast.Constant 1)], 
                             Ast.Binop (Ast.Ident "i", Ast.Eq, Ast.Constant 0))]) ]) in

  let fwr_updated_store_program = 
    (["x"; "y"; "i"], 
    [ ("increment_x", [Ast.ModStmt ("x", Ast.Plus, Ast.Constant 2)]);
      ("decrement_y", [Ast.ModStmt ("y", Ast.Min, Ast.Constant 1)]);
      ("unmain", [Ast.LoopStmt (Ast.Binop (Ast.Ident "i", Ast.Eq, Ast.Constant 0),
                   [Ast.ModStmt ("i", Ast.Plus, Ast.Constant 1); Ast.UncallStmt "increment_x"; Ast.UncallStmt "decrement_y"], 
                   Ast.Binop (Ast.Ident "i", Ast.Eq, Ast.Constant 3));
                 Ast.ModStmt ("i", Ast.Min, Ast.Constant 3);
                 Ast.UncallStmt "increment_x"]) ]) in

  let updated_store = feval fwr_program in
  let beval_store = beval fwr_updated_store_program in
  updated_store = beval_store

let%test "invert program and apply forward on updated store to get back initial store" =
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
    B.List.Assoc.find final_store ~equal:String.equal "x1" = Some 5 &&
    B.List.Assoc.find final_store ~equal:String.equal "x2" = Some 8 &&
    B.List.Assoc.find final_store ~equal:String.equal "n" = Some 0

let%test "test feval on doublebit.jsub" =
  let program_str = read_file "../../../../../examples/doublebit.jsub" in
  let lexbuf = Lexing.from_string program_str in
  let program = parse lexbuf in
  let final_store = feval program in
  B.List.Assoc.find final_store ~equal:String.equal "z" = Some 0 &&
  B.List.Assoc.find final_store ~equal:String.equal "bit" = Some 0 &&
  B.List.Assoc.find final_store ~equal:String.equal "n" = Some 4

let%test "test feval on occur-check.jsub" =
  let program_str = read_file "../../../../../examples/occurs-check.jsub" in
  let lexbuf = Lexing.from_string program_str in
    let program = parse lexbuf in
    let final_store = feval program in
    B.List.Assoc.find final_store ~equal:String.equal "x" = Some 0

let test_invert_with_feval_and_beval file_path =
  let lexbuf = Lexing.from_string (read_file file_path) in
  let program = parse lexbuf in
  let updated_store = feval program in
  let inverted_program = invert program in
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

let%test "constant folding addition of constants" =
  let expr = Ast.Binop (Ast.Constant 3, Ast.Plus, Ast.Constant 5) in
  let folded_expr = constant_fold_expr expr in
  match folded_expr with
  | Ast.Constant result -> result = 8
  | _ -> false

let%test "constant folding subtraction of identical variables" =
  let expr = Ast.Binop (Ast.Ident "x", Ast.Min, Ast.Ident "x") in
  let folded_expr = constant_fold_expr expr in
  match folded_expr with
  | Ast.Constant result -> result = 0
  | _ -> false

let%test "constant folding multiplication by zero" =
  let expr = Ast.Binop (Ast.Constant 0, Ast.Mult, Ast.Ident "x") in
  let folded_expr = constant_fold_expr expr in
  match folded_expr with
  | Ast.Constant result -> result = 0
  | _ -> false

let%test "constant fold stmt - simple modification" =
  let open Ast in
  let stmt = ModStmt ("x", Plus, Binop (Constant 4, Plus, Constant 5)) in
  let optimized_stmt = constant_fold_stmt stmt in
  match optimized_stmt with
  | ModStmt (_, _, Constant value) -> value = 9
  | _ -> false

let%test "constant fold stmt - if statement with constant condition" =
  let open Ast in
  let stmt = IfStmt (Binop (Constant 1, Eq, Constant 1),
                      [ModStmt ("x", Plus, Constant 1)],
                      [ModStmt ("x", Min, Constant 1)],
                      Constant 0) in
  let optimized_stmt = constant_fold_stmt stmt in
  match optimized_stmt with
  | IfStmt (Constant value, then_branch, else_branch, _) ->
      value = 1 &&
      (match then_branch with
        | [ModStmt (_, _, Constant v)] -> v = 1
        | _ -> false) &&
      (match else_branch with
        | [ModStmt (_, _, Constant v)] -> v = 1
        | _ -> false)
  | _ -> false

let%test "constant fold stmt - loop statement with optimizable test" =
  let open Ast in
  let stmt = LoopStmt (Constant 1,
                       [ModStmt ("x", Plus, Constant 1)],
                       Binop (Constant 10, Eq, Constant 10)) in
  let optimized_stmt = constant_fold_stmt stmt in
  match optimized_stmt with
  | LoopStmt (_, loop_body, Constant value) ->
      value = 1 &&
      (match loop_body with
       | [ModStmt (_, _, Constant v)] -> v = 1
       | _ -> false)
  | _ -> false

let%test "eliminate dead code in if-statement with false constant condition" =
  let open Ast in
  let stmt = IfStmt (Constant 0,
                      [ModStmt ("x", Plus, Constant 1)],
                      [ModStmt ("x", Min, Constant 1)],
                      Constant 0) in
  match dead_code_elim_stmt stmt with
  | [ModStmt (_, Min, Constant 1)] -> true
  | _ -> false

let%test "retain code in if-statement with non-constant condition" =
  let open Ast in
  let stmt = IfStmt (Ident "condition",
                      [ModStmt ("x", Plus, Constant 1)],
                      [ModStmt ("x", Min, Constant 1)],
                      Constant 4) in
  match dead_code_elim_stmt stmt with
  | [IfStmt (Ident "condition", _, _, _)] -> true
  | _ -> false

let%test "error on loop with always true condition" =
  let open Ast in
  let stmt = LoopStmt (Constant 1,
                       [ModStmt ("x", Plus, Constant 1)],
                       Constant 1) in
  try
    let _ = dead_code_elim_stmt stmt in
    false
  with
  | Failure _ -> true
  | _ -> false

let%test "error on loop with always false test" =
  let open Ast in
  let stmt = LoopStmt (Ident "condition",
                       [ModStmt ("x", Plus, Constant 1)],
                       Constant 0) in
  try
    let _ = dead_code_elim_stmt stmt in
    false
  with
  | Failure _ -> true
  | _ -> false