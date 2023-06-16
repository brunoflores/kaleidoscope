%{
%}

%token <string> ID
%token <int> INT
%token DEF
%token EXTERN
%token EOF

%start <unit> prog

%%

prog:
  | EOF { () }
