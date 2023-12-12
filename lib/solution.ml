type program = Ast.program
type store = (Ast.identifier * int) list

let parse      = Parser.program Lexer.token (* Do not change *)

let update_store (id : Ast.identifier) (value : int) (store : store) : store =
  (id, value) :: List.remove_assoc id store
  
let string_of_store (store : store) : string =
  let pair_to_string (id, value) = id ^ ": " ^ string_of_int value in
  String.concat ", " (List.map pair_to_string store)

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

let eval_binop (op : Ast.binop) (v1 : int) (v2 : int) (forward : bool) : int =
  match op with
  | Ast.Plus -> if forward then v1 + v2 else v1 - v2
  | Ast.Min -> if forward then v1 - v2 else v1 + v2
  | Ast.Mult -> if forward then v1 * v2 else v1 / v2
  | Ast.Div -> if forward then v1 / v2 else v1 * v2
  | Ast.Eq -> if v1 = v2 then 1 else 0
  | Ast.NEq -> if v1 <> v2 then 1 else 0

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
    

let rec feval(program: program) : store =
  match program with
  | _, [] -> failwith "No procedures found in the program"
  | _, procedures ->
    let initial_store = List.map (fun id -> (id, 0)) (fst program) in
    Printf.printf "Initial Store: %s\n" (string_of_store initial_store);
    execute_procedure (List.hd (List.rev procedures)) initial_store procedures true

    (*Execute the last procedure*)
  and execute_procedure (procedure: Ast.procedure) (store : store) (all_procedures : Ast.procedure list) (forward : bool) : store =
    let proc_id, statements = procedure in
    Printf.printf "Executing procedure %s\n" proc_id;
    if forward then
      execute_statements statements store all_procedures true
    else
      execute_statements (List.rev statements) store all_procedures false
    
  and execute_statements (statements : Ast.statement list) (store: store) (all_procedures : Ast.procedure list) (forward : bool) : store =
    match statements with
    | [] -> store
    | statement :: rest ->
      if forward then
        let new_store = statement_execution statement store all_procedures true in
        execute_statements rest new_store all_procedures true
      else
        let new_store = statement_execution statement store all_procedures false in
        execute_statements rest new_store all_procedures false

  and statement_execution (statement : Ast.statement) (store : store) (all_procedures : Ast.procedure list) (forward : bool) : store =
    Printf.printf "Current Store: %s\n" (string_of_store store);
    match statement with
    | Ast.ModStmt (id, modi, expr) ->
      let current_value = List.assoc id store in
      let result = eval_expr expr store forward in
      let update_value = eval_binop modi current_value result forward in
      update_store id update_value store
  
    | Ast.SwapStmt (id1, id2) ->
      let value1 = List.assoc id1 store in
      let value2 = List.assoc id2 store in
      let updated_store = update_store id1 value2 (update_store id2 value1 store) in
      updated_store

    | Ast.IfStmt (cond, then_stmts, else_stmts, assertion) ->
      Printf.printf "Store at if stmt: %s\n" (string_of_store store);
      if (forward && eval_expr cond store forward <> 0) || (not forward && eval_expr assertion store forward <> 0) then
        begin
          let updated_store = if forward then execute_statements then_stmts store all_procedures true
                              else execute_statements (List.rev then_stmts) store all_procedures false in
          Printf.printf "Updated store in then stmt: %s\n" (string_of_store updated_store);
          if (forward && eval_expr assertion updated_store forward<> 0) || (not forward && eval_expr cond updated_store forward <> 0) then
            updated_store
          else
            failwith "Assertion failed after executing then-branch"
        end
      else
        begin
          let updated_store = if forward then execute_statements else_stmts store all_procedures true
                              else execute_statements (List.rev else_stmts) store all_procedures false in
          Printf.printf "Updated store in else stmt: %s\n" (string_of_store updated_store);
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
      loop_execution assertion loop_body test store all_procedures forward
    | _ -> failwith ("Unsupported statement type: " ^ Ast.show_statement statement)

  and execute_procedure_by_id (procedure_name : Ast.identifier) (store : store) (all_procedures : Ast.procedure list) (forward : bool) : store =
      match find_procedure_by_id procedure_name all_procedures with
      | Some stmts ->
        let statements = if forward then stmts else List.rev stmts in
        execute_statements statements store all_procedures forward
      | None -> failwith ("Procedure not found: " ^ procedure_name)

  and loop_execution (assertion : Ast.expression) (loop_body : Ast.statement list) (test : Ast.expression) (store : store) (all_procedures : Ast.procedure list) (forward : bool) : store =
    if (forward && eval_expr assertion store forward<> 0) || (not forward && eval_expr test store forward<> 0) then
      let rec loop (store : store) : store =
          let updated_store = if forward then execute_statements loop_body store all_procedures true
                              else execute_statements (List.rev loop_body) store all_procedures false in
          Printf.printf "Updated Store during loop: %s\n" (string_of_store updated_store); (* Print updated store *)
          if (forward && eval_expr test updated_store forward == 0) || (not forward && eval_expr assertion updated_store forward == 0) then
            loop updated_store
          else
            updated_store
        in
      loop store
    else
      failwith "Loop assertion failed initially"
(*-----------------------------------------------------*)

let beval(program: program) : store =
  match program with
  | _, [] -> failwith "No procedures found in the program"
  | _, procedures ->
    let initial_store = List.map (fun id -> (id, 0)) (fst program) in
    Printf.printf "Initial Store: %s\n" (string_of_store initial_store);
    execute_procedure (List.hd (List.rev procedures)) initial_store procedures false
        
(*-----------------------------------------------------*)
let invert     = fun _ -> failwith "implement me!"
let optimize   = fun _ ->
  let constant_fold  = fun _ -> failwith "implement me" in
  let dead_code_elim = fun _ -> failwith "implement me" in
  let proc_inline    = fun _ -> failwith "implement me" in
  failwith "implement me!"

let string_of_program = Ast.string_of_program (* Do not change *)