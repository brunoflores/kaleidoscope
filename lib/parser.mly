%{

open Ast

let pos_of_lexing_position (pos : Lexing.position) : pos =
  { pos_fname = pos.pos_fname;
    pos_lnum = pos.pos_lnum;
    pos_bol = pos.pos_bol;
    pos_cnum = pos.pos_cnum }

%}

%token <string> ID
%token <int> INT
%token IF
%token THEN
%token ELSE
%token DEF
%token EXTERN
%token LPAREN
%token RPAREN
%token LESSTHAN
%token MINUS
%token PLUS
%token EOF

%start <prog> prog

%%

prog:
  | t = top+; EOF { t }
  | EOF { failwith "Empty" }

top:
  | e = exp { TopExp e }
  | d = dec { TopDec d }

dec:
  | DEF; name = ID; LPAREN; arg = ID; RPAREN; body = exp
    { FunDec {
        name;
        params = [arg];
        body;
        pos = pos_of_lexing_position $startpos } }

exp:
  | c = constant { c }
  | IF; antecedent = exp; THEN; consequent = exp; ELSE; alternative = exp
    { IfExp {
        antecedent;
        consequent;
        alternative = Some alternative;
        pos = pos_of_lexing_position $startpos } }

constant:
  | c = INT { IntExp c }
