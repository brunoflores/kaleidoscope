module L = Llvm

let ctx = L.create_context ()
let builder = L.builder ctx
let topmod = L.create_module ctx "top"
let namedvals : (string * L.llvalue) list = []

(* -- *)

let rec genexp = function
  | Ast.IntExp i ->
      let ty = L.float_type ctx in
      let f = float_of_int i in
      L.const_float ty f
  | Ast.VarExp (Ast.SimpleVar v) -> List.assoc v namedvals
  | Ast.BinExp { left; op; right } -> (
      let left = genexp left in
      let right = genexp right in
      match op with
      | Ast.PlusOp -> L.build_fadd left right "addtmp" builder
      | Ast.MinusOp -> L.build_fsub left right "subtmp" builder
      | _ -> failwith "not implemented")
  | Ast.CallExp { id; args = caller_args; pos = _ } -> (
      match L.lookup_function id topmod with
      | Some f ->
          let callee_args = L.params f in
          let _ =
            if Array.length callee_args != List.length caller_args then
              failwith "arguments number mismatch"
          in
          let arg_vals = Array.of_list @@ List.map genexp caller_args in
          L.build_call f arg_vals "calltmp" builder
      | None -> failwith "undefined function call")
  | _ -> failwith "not implemented"

and gendec = function
  | Ast.FunDec { name; params; body = _; pos = _ } ->
      let double = L.double_type ctx in
      let n = List.length params in
      let argtys = Array.make n double in
      let rt = L.function_type double argtys in
      let fn = L.define_function name rt topmod in
      fn

let gentop top =
  let _ =
    List.map
      (fun i ->
        match i with Ast.TopExp e -> genexp e | Ast.TopDec d -> gendec d)
      top
  in
  L.string_of_llmodule topmod
