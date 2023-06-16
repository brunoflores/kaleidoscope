{

open Parser

exception SyntaxError of string

let next_line lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
                           pos_bol  = lexbuf.lex_curr_pos;
                           pos_lnum = pos.pos_lnum + 1 }

}

let digit = ['0'-'9']
let int = digit digit*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "def"    { DEF }
  | "extern" { EXTERN }
  | id       { ID (Lexing.lexeme lexbuf) }
  | _        { raise (SyntaxError ("Unexpected: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
