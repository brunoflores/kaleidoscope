type pos = { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
[@@deriving show]

type prog = top list [@@deriving show]
and top = TopExp of exp | TopDec of dec [@@deriving show]

and exp =
  | IntExp of int
  | BinExp of { left : exp; op : op; right : exp }
  | VarExp of var
  | CallExp of { id : string; args : exp list; pos : pos }
  | IfExp of {
      antecedent : exp;
      consequent : exp;
      alternative : exp option;
      pos : pos;
    }
[@@deriving show]

and dec =
  | FunDec of { name : string; params : string list; body : exp; pos : pos }
[@@deriving show]

and op = PlusOp | MinusOp | LtOp [@@deriving show]
and var = SimpleVar of string [@@deriving show]
