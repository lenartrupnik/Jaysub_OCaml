type program = Ast.program
type store = (Ast.identifier * int) list

let parse      = Parser.program Lexer.token (* Do not change *)

let string_of_store (store : store) : string =
  let pair_to_string (id, value) = id ^ ": " ^ string_of_int value in
  String.concat ", " (List.map pair_to_string store)

let rec feval(program: program) : store =
  match program with
  | _, [] -> failwith "No procedures found in the program"
  | _, procedures ->
    let initial_store = List.map (fun id -> (id, 0)) (fst program) in
    let procedure_table = Hashtbl.create 10 in (* Create the procedure table *)
    Printf.printf "Initial Store: %s\n" (string_of_store initial_store);

    (* Register procedures in the procedure_table *)
    List.iter (fun (id, stmts) -> register_procedure procedure_table id stmts) procedures;

    execute_procedure procedure_table (List.hd (List.rev procedures)) initial_store

  and execute_procedure (procedure_table: (Ast.identifier, Ast.statement list) Hashtbl.t) (procedure: Ast.procedure) (store : store) : store =
    let proc_id, statements = procedure in
    Printf.printf "Executing procedure %s\n" proc_id;
    execute_statements procedure_table statements store
  
  and execute_statements (procedure_table: (Ast.identifier, Ast.statement list) Hashtbl.t) (statements : Ast.statement list) (store: store) : store =
    match statements with
    | [] -> store
    | statement :: rest ->
      let new_store = execute_statement procedure_table statement store in
      Printf.printf "Executing statement: %s\n" (Ast.show_statement statement);
      Printf.printf "Current Store: %s\n" (string_of_store store);
      execute_statements procedure_table rest new_store
  
  and execute_statement (procedure_table: (Ast.identifier, Ast.statement list) Hashtbl.t) (statement : Ast.statement) (store : store) : store =
    match statement with
    | Ast.ModStmt (id, modi, expr) ->
      let current_value = List.assoc id store in
      let result = eval_expr expr store in
      let update_value = apply_binop modi current_value result in
      update_store id update_value store
  
    | Ast.SwapStmt (id1, id2) ->
      let value1 = List.assoc id1 store in
      let value2 = List.assoc id2 store in
      let updated_store = update_store id1 value2 (update_store id2 value1 store) in
      updated_store
  
    | Ast.IfStmt (cond, then_stmts, else_stmts, assertion) ->
      if eval_expr cond store <> 0 then
        let updated_store = execute_statements procedure_table then_stmts store in
        let assertion_value = eval_expr assertion updated_store in
        if assertion_value <> 0 then
          updated_store
        else
          failwith "Assertion failed after executing then-branch"
      else
        let updated_store = execute_statements procedure_table else_stmts store in
        let assertion_value = eval_expr assertion updated_store in
        if assertion_value <> 0 then
          failwith "Assertion failed after executing else-branch"
        else
          updated_store
  
    | Ast.CallStmt procedure_name ->
      execute_procedure_call procedure_table procedure_name store
  
    | Ast.UncallStmt procedure_name ->
      execute_inverse_procedure procedure_table procedure_name store
    
    | Ast.LoopStmt (assertion, loop_body, test) ->
      execute_loop procedure_table assertion loop_body test store
    | _ -> failwith ("Unsupported statement type: " ^ Ast.show_statement statement)
  
  and execute_procedure_call (procedure_table: (Ast.identifier, Ast.statement list) Hashtbl.t) (procedure_name : Ast.identifier) (store : store) : store =
    match Hashtbl.find_opt procedure_table procedure_name with
    | Some stmts -> execute_statements procedure_table stmts store
    | None -> failwith ("Procedure not found: " ^ procedure_name)

  and execute_inverse_procedure (procedure_table: (Ast.identifier, Ast.statement list) Hashtbl.t) (procedure_name : Ast.identifier) (store : store) : store =
    match Hashtbl.find_opt procedure_table procedure_name with
    | Some stmts -> execute_statements procedure_table (List.rev stmts) store
    | None -> failwith ("Procedure not found: " ^ procedure_name)
    
  and eval_expr (expr : Ast.expression) (store : store) : int =
    let print_expr expr_str = Printf.printf "Evaluating expression: %s\n" expr_str in
    match expr with
    | Ast.Constant c -> print_expr (string_of_int c); c
    | Ast.Ident id -> print_expr id; List.assoc id store
    | Ast.Binop (e1, op, e2) ->
      let v1 = eval_expr e1 store in
      let v2 = eval_expr e2 store in
      let result = apply_binop op v1 v2 in
      print_expr (Ast.string_of_expression expr ^ " = " ^ string_of_int result);
      result
  and apply_binop (op : Ast.binop) (v1 : int) (v2 : int) : int =
    match op with
    | Ast.Plus -> v1 + v2
    | Ast.Min -> v1 - v2
    | Ast.Mult -> v1 * v2
    | Ast.Div -> v1 / v2
    | Ast.Eq -> if v1 = v2 then 1 else 0
    | Ast.NEq -> if v1 <> v2 then 1 else 0

  and update_store (id : Ast.identifier) (value : int) (store : store) : store =
    (id, value) :: List.remove_assoc id store

  and register_procedure (procedure_table: (Ast.identifier, Ast.statement list) Hashtbl.t) (name : Ast.identifier) (body : Ast.statement list) : unit =
    Hashtbl.add procedure_table name body

  and execute_loop (procedure_table: (Ast.identifier, Ast.statement list) Hashtbl.t) (assertion : Ast.expression) (loop_body : Ast.statement list) (test : Ast.expression) (store : store) : store =
    if eval_expr assertion store <> 0 then
      let rec loop (store : store) : store =
          let updated_store = execute_statements procedure_table loop_body store in
          Printf.printf "Updated Store during loop: %s\n" (string_of_store updated_store); (* Print updated store *)
          if eval_expr test updated_store == 0 then
            loop updated_store
          else
            updated_store
        in
      loop store
      else
        failwith "Loop assertion failed initially"

(*-----------------------------------------------------*)
let rec beval(program: program) : store =
  match program with
  | _, [] -> failwith "No procedures found in the program"
  | _, procedures ->
    let initial_store = List.map (fun id -> (id, 0)) (fst program) in
    let procedure_table = Hashtbl.create 10 in (* Create the procedure table *)
    Printf.printf "Initial Store: %s\n" (string_of_store initial_store);

    (* Register procedures in the procedure_table *)
    List.iter (fun (id, stmts) -> register_procedure procedure_table id stmts) procedures;

    execute_procedure procedure_table (List.hd procedures) initial_store

  and execute_procedure (procedure_table: (Ast.identifier, Ast.statement list) Hashtbl.t) (procedure: Ast.procedure) (store : store) : store =
    let proc_id, statements = procedure in
    Printf.printf "Executing procedure %s (backward)\n" proc_id;
    execute_statements procedure_table (List.rev statements) store

  and execute_statements (procedure_table: (Ast.identifier, Ast.statement list) Hashtbl.t) (statements : Ast.statement list) (store: store) : store =
    match statements with
    | [] -> store
    | statement :: rest ->
      let new_store = execute_statement procedure_table statement store in
      Printf.printf "Executing statement: %s (backward)\n" (Ast.show_statement statement);
      Printf.printf "Current Store: %s\n" (string_of_store store);
      execute_statements procedure_table rest new_store

  and execute_statement (procedure_table: (Ast.identifier, Ast.statement list) Hashtbl.t) (statement : Ast.statement) (store : store) : store =
    match statement with
    | Ast.ModStmt (id, modi, expr) ->
      let current_value = List.assoc id store in
      let result = eval_expr expr store in
      let update_value = reverse_apply_binop modi current_value result in
      update_store id update_value store

    | Ast.SwapStmt (id1, id2) ->
      let value1 = List.assoc id1 store in
      let value2 = List.assoc id2 store in
      let updated_store = update_store id1 value2 (update_store id2 value1 store) in
      updated_store

    | Ast.IfStmt (cond, then_stmts, else_stmts, assertion) ->
      if eval_expr cond store <> 0 then
        let updated_store = execute_statements procedure_table then_stmts store in
        let assertion_value = eval_expr assertion updated_store in
        if assertion_value <> 0 then
          updated_store
        else
          failwith "Assertion failed after executing then-branch (backward)"
      else
        let updated_store = execute_statements procedure_table else_stmts store in
        let assertion_value = eval_expr assertion updated_store in
        if assertion_value <> 0 then
          failwith "Assertion failed after executing else-branch (backward)"
        else
          updated_store

    | Ast.CallStmt procedure_name ->
      execute_procedure_call procedure_table procedure_name store

    | Ast.UncallStmt procedure_name ->
      execute_inverse_procedure procedure_table procedure_name store

    | Ast.LoopStmt (assertion, loop_body, test) ->
      execute_loop procedure_table assertion loop_body test store

    | _ -> failwith ("Unsupported statement type: " ^ Ast.show_statement statement)

  and reverse_apply_binop (op : Ast.binop) (result : int) (current_value : int) : int =
    match op with
    | Ast.Plus -> result - current_value
    | Ast.Min -> current_value + result
    | Ast.Mult -> if current_value = 0 then 0 else result / current_value
    | Ast.Div -> if current_value = 0 then 0 else result * current_value
    | Ast.Eq -> 1
    | Ast.NEq -> 1

  and update_store (id : Ast.identifier) (value : int) (store : store) : store =
    (id, value) :: List.remove_assoc id store

  and register_procedure (procedure_table: (Ast.identifier, Ast.statement list) Hashtbl.t) (name : Ast.identifier) (body : Ast.statement list) : unit =
    Hashtbl.add procedure_table name body

  and execute_loop (procedure_table: (Ast.identifier, Ast.statement list) Hashtbl.t) (assertion : Ast.expression) (loop_body : Ast.statement list) (test : Ast.expression) (store : store) : store =
    if eval_expr test store <> 0 then
      let rec loop (store : store) : store =
          let updated_store = execute_statements procedure_table loop_body store in
          Printf.printf "Updated Store during loop: %s\n" (string_of_store updated_store); (* Print updated store *)
          if eval_expr assertion updated_store == 0 then
            loop updated_store
          else
            updated_store
        in
      loop store
      else
        failwith "Loop assertion failed initially"
        
(*-----------------------------------------------------*)
let invert     = fun _ -> failwith "implement me!"
let optimize   = fun _ ->
  let constant_fold  = fun _ -> failwith "implement me" in
  let dead_code_elim = fun _ -> failwith "implement me" in
  let proc_inline    = fun _ -> failwith "implement me" in
  failwith "implement me!"

let string_of_program = Ast.string_of_program (* Do not change *)