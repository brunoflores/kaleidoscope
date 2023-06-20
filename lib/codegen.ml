module L = Llvm

let ctx = L.create_context ()
let topmod = L.create_module ctx "top"

(* -- *)

let rec genexp (env : (string * L.llvalue) list) (b : L.llbuilder) = function
  | Ast.IntExp i ->
      let ty = L.float_type ctx in
      let f = float_of_int i in
      L.const_float ty f
  | Ast.VarExp (Ast.SimpleVar id) -> List.assoc id env
  | Ast.BinExp { left; op; right } -> (
      let left = genexp env b left in
      let right = genexp env b right in
      match op with
      | Ast.PlusOp -> L.build_fadd left right "addtmp" b
      | Ast.MinusOp -> L.build_fsub left right "subtmp" b
      | _ -> failwith "not implemented")
  | Ast.CallExp { id; args = caller_args; pos = _ } -> (
      match L.lookup_function id topmod with
      | Some f ->
          let callee_args = L.params f in
          let _ =
            if Array.length callee_args != List.length caller_args then
              failwith "arguments number mismatch"
          in
          let arg_vals = Array.of_list @@ List.map (genexp env b) caller_args in
          let _block = L.insertion_block b in
          L.build_call f arg_vals "calltmp" b
      | None -> failwith @@ Format.sprintf "undefined function call: %s" id)
  | _ -> failwith "not implemented"

and gendec (b : L.llbuilder) = function
  | Ast.FunDec { name; params; body; pos = _ } ->
      let double = L.double_type ctx in
      let n = List.length params in
      let argtys = Array.make n double in
      let rt = L.function_type double argtys in
      let fn = L.define_function name rt topmod in
      let body = genexp [] b body in
      let entry = L.entry_block fn in
      let builder = L.builder_at_end ctx entry in
      let _ = L.build_ret body builder in
      fn

let gentop top =
  (* let ty = L.float_type ctx in *)
  (* let v = float_of_int 42 in *)
  (* let v = L.const_float ty v in *)
  (* let _ = L.build_ret v builder in *)
  let mainrt = L.function_type (L.double_type ctx) [||] in
  let main = L.define_function "main" mainrt topmod in
  let entry = L.entry_block main in
  let builder = L.builder_at_end ctx entry in
  let _ =
    List.iter
      (fun i ->
        match i with
        | Ast.TopExp e ->
            let _ = genexp [] builder e in
            ()
        | Ast.TopDec d ->
            let _ = gendec builder d in
            ())
      top
  in
  L.string_of_llmodule topmod
