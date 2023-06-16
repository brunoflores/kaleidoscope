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
%token EOF

%start <exp option> prog

%%

prog:
  | e = exp; EOF { Some e }
  | EOF { None }

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
