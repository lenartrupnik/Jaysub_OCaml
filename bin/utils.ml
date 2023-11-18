let print_position outx lexbuf =
  let open Lexing in
  let open Printf in
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let (<<) f g x = f(g(x))
