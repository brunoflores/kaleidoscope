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
%token LPAREN
%token RPAREN
%token LESSTHAN
%token MINUS
%token PLUS
%token TIMES
%token COMMA
%token EOF

// Priorities from lower to higher linewise
%left PLUS MINUS LESSTHAN
%left TIMES
%nonassoc ID
%left LPAREN

%start <prog> prog

%%

prog:
  | t = top*; EOF { t }

top:
  | e = eitherexp { TopExp e }
  | d = dec { TopDec d }

eitherexp:
  | e = openexp { e }
  | e = closedexp { e }

dec:
  | DEF; name = ID; LPAREN; args = separated_list(COMMA, ID); RPAREN; body = closedexp
    { FunDec {
        name;
        params = args;
        body;
        pos = pos_of_lexing_position $startpos } }

exp:
  | c = constant { c }
  | b = binop { b }
  | x = lvalue { VarExp x }
  | LPAREN; e = exp; RPAREN { e }
  | id = ID; LPAREN; args = exprlist; RPAREN
    { CallExp { id; args; pos = pos_of_lexing_position $startpos } }

openexp:
  | IF; antecedent = eitherexp;
    THEN; consequent = exp
    { IfExp {
        antecedent;
        consequent;
        alternative = None;
        pos = pos_of_lexing_position $startpos } }
  | IF; antecedent = eitherexp;
    THEN; consequent = openexp
    { IfExp {
        antecedent;
        consequent;
        alternative = None;
        pos = pos_of_lexing_position $startpos } }
  | IF; antecedent = eitherexp;
    THEN; consequent = closedexp;
    ELSE; alternative = openexp
    { IfExp {
        antecedent;
        consequent;
        alternative = Some alternative;
        pos = pos_of_lexing_position $startpos } }

closedexp:
  | e = exp { e }
  | IF; antecedent = eitherexp;
    THEN; consequent = closedexp;
    ELSE; alternative = closedexp
    { IfExp {
        antecedent;
        consequent;
        alternative = Some alternative;
        pos = pos_of_lexing_position $startpos } }

constant:
  | c = INT { IntExp c }

binop:
  | left = exp; LESSTHAN; right = exp
    { BinExp { left; op = LtOp; right} }
  | left = exp; PLUS; right = exp
    { BinExp { left; op = PlusOp; right} }
  | left = exp; MINUS; right = exp
    { BinExp { left; op = MinusOp; right} }
  | left = exp; TIMES; right = exp
    { BinExp { left; op = TimesOp; right } }

lvalue:
  | id = ID { SimpleVar id }

exprlist:
  | l = separated_list(COMMA, eitherexp) { l }
