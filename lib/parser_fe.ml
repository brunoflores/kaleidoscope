module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Parser.MenhirInterpreter

let print_position outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  let fname = pos.pos_fname in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.fprintf outx "%s:%d:%d" fname line col

let env checkpoint =
  match checkpoint with I.HandlingError env -> env | _ -> assert false

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None ->
      (* Hmm... The parser is in its initial state. The incremental API
         currently lacks a way of finding out the number of the initial
         state. It is usually 0, so we return 0. This is unsatisfactory
         and should be fixed in the future. *)
      0

let show text positions =
  E.extract text positions |> E.sanitize |> E.compress |> E.shorten 20
(* max width 43 *)

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
          keywords are correctly inside the syntax error message
          database. The integer [i] should always be a valid offset
          into the known suffix of the stack. *)
      failwith "index out of range (from Menhir): see the source code"

(*
let pretty_print_err = function
  | Some { Ast.pos_fname; pos_lnum; _ }, message -> (
      let ic = open_in pos_fname in
      let lnum = ref pos_lnum in
      let line = ref "" in
      try
        while !lnum > 0 do
          line := input_line ic;
          decr lnum
        done;
        Printf.printf "File \"%s\"\n%d |%s\n\nError: %s\n" pos_fname pos_lnum
          !line message;
        close_in ic
      with e ->
        close_in_noerr ic;
        raise e)
  | None, message -> print_endline message
*)

let succeed prog =
  (* let _ = Format.printf "%a\n\n" Ast.pp_prog prog in *)
  let code = Codegen.gentop prog in
  print_endline code

let fail text buffer checkpoint =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication =
    Format.sprintf "Syntax error %s.\n" (E.show (show text) buffer)
  in
  (* Fetch an error message from the database. *)
  let message = ParserMessages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  Printf.eprintf "%s%s%s%!" location indication message;
  exit 1

let parse lexbuf text =
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.prog lexbuf.lex_curr_p in
  try I.loop_handle succeed (fail text buffer) supplier checkpoint
  with Lexer.SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    ()

let get_contents s =
  let open Stdio in
  let filename, content =
    match s with
    | None | Some "-" -> ("-", In_channel.input_all In_channel.stdin)
    | Some filename -> (filename, In_channel.read_all filename)
  in
  (L.init filename (content |> Lexing.from_string), content)

let compile (files : string list) : unit =
  let filename = List.hd files in
  let lexbuf, content = get_contents (Some filename) in
  parse lexbuf content
