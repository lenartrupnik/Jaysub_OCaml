(** This file represents the lexer source code.
    Do not change it unless explicitly told to do so *)
{
  open Parser

  exception Error of string

}

let white = [' ' '\t']+
let digit = ['0'-'9']
let sign  = ['-' '+']
let alpha = ['a'-'z' 'A'-'Z']
let eol   = ['\n']

let int_constant = sign? digit+
let identifier = alpha (alpha | digit | '_' | '-')*

rule token = parse
  | white { token lexbuf }
  | "procedure" { PROCEDURE }
  | "call" { CALL }
  | "uncall" { UNCALL }
  | "skip" { SKIP }
  | "<=>" { SWAP }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fi" { FI }
  | "from" { FROM }
  | "do" { DO }
  | "until" { UNTIL }
  | "{" { CURL_START }
  | "}" { CURL_END }
  | "(" { PAREN_START }
  | ")" { PAREN_END }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "+=" { PLUS_EQ }
  | "-=" { NEG_EQ }
  | "*=" { TIMES_EQ }
  | "/=" { DIV_EQ }
  | "==" { EQ }
  | "!=" { NEQ }
  | "+" { PLUS }
  | "-" { MIN }
  | "*" { MULT }
  | "/" { DIV }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  | int_constant { INT_CONSTANT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
