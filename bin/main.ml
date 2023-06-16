open Kaleidoscope

let () =
  let usage = "Compile" in
  let spec = [] in
  let files = ref [] in
  let readfname s = files := s :: !files in
  Arg.parse spec readfname usage;
  Parser_fe.compile !files
