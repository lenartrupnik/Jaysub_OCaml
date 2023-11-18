%{
%}

%token <int> INT_CONSTANT
%token <string> IDENTIFIER
%token PROCEDURE CALL UNCALL SWAP IF THEN ELSE FI SKIP FROM DO UNTIL
%token EQ NEQ
%token PLUS_EQ NEG_EQ TIMES_EQ DIV_EQ
%token PLUS MIN MULT DIV
%token CURL_START CURL_END PAREN_START PAREN_END SEMICOLON COMMA
%token EOF
%left PLUS MIN
%left MULT DIV

%start <Ast.program> program
%%

program:
    | decl = separated_list(COMMA, IDENTIFIER); SEMICOLON; procs = list(procedure) EOF { decl, procs }
;
procedure:
    | PROCEDURE; procname = IDENTIFIER; 
      CURL_START;
      s = statements;
      CURL_END { procname, s }
;
statements:
    | stmts = list(statement)  { stmts }
;
statement:
    | CALL; id = IDENTIFIER; SEMICOLON { Ast.CallStmt id }
    | UNCALL; id = IDENTIFIER; SEMICOLON { Ast.UncallStmt id }
    | id = IDENTIFIER; modi = modifier; rval = expression; SEMICOLON
         { Ast.ModStmt (id, modi, rval) }
    | lid = IDENTIFIER; SWAP; rid = IDENTIFIER; SEMICOLON
         { Ast.SwapStmt (lid, rid) }
    | IF; PAREN_START; cnd = condition; PAREN_END;
      THEN; CURL_START; con = list(statement); CURL_END;
      ELSE; CURL_START; alt = list(statement); CURL_END;
      FI; PAREN_START; ass = condition; PAREN_END;
         { Ast.IfStmt (cnd, con, alt, ass) }
    | SKIP; SEMICOLON
         { Ast.SkipStmt }
    | FROM; PAREN_START; test = condition; PAREN_END;
      DO;   CURL_START; dobody   = list(statement); CURL_END;
      UNTIL PAREN_START; ass = condition; PAREN_END;
        { Ast.LoopStmt (test, dobody, ass) }
;
modifier:
    | PLUS_EQ { Ast.Plus }
    | NEG_EQ { Ast.Min }
    | TIMES_EQ { Ast.Mult }
    | DIV_EQ { Ast.Div }
;
expression:
    | id = IDENTIFIER { Ast.Ident id }
    | i = INT_CONSTANT { Ast.Constant i }
    | lid = expression; PLUS; rid = expression { Ast.Binop (lid, Ast.Plus, rid) }
    | lid = expression; MIN;  rid = expression { Ast.Binop (lid, Ast.Min, rid) }
    | lid = expression; MULT; rid = expression { Ast.Binop (lid, Ast.Mult, rid) }
    | lid = expression; DIV;  rid = expression { Ast.Binop (lid, Ast.Div, rid) }
;
condition: lid = expression; b = binop_cond ; rid = expression { Ast.Binop (lid, b, rid) }
;
binop_cond:
    | EQ { Ast.Eq } | NEQ { Ast.NEq }
