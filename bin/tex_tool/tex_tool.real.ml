open Abella2tex

let available = true

(* Redirect stderr to a pipe for the duration of [f], returning what was
   written there alongside the result. abella2tex's Render warns (e.g. its
   variable-injectivity check) go to stderr, which an MCP client cannot see
   unless it is folded into the tool's own text result. *)
let capture_stderr (f : unit -> 'a) : string * 'a =
  flush stderr;
  let read_fd, write_fd = Unix.pipe ~cloexec:false () in
  let saved = Unix.dup Unix.stderr in
  Unix.dup2 write_fd Unix.stderr;
  Unix.close write_fd;
  let result = f () in
  flush stderr;
  Unix.dup2 saved Unix.stderr;
  Unix.close saved;
  Unix.set_nonblock read_fd;
  let buf = Buffer.create 256 in
  let bytes = Bytes.create 4096 in
  (try
     let rec drain () =
       let n = Unix.read read_fd bytes 0 (Bytes.length bytes) in
       if n > 0 then (
         Buffer.add_subbytes buf bytes 0 n;
         drain ())
     in
     drain ()
   with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ());
  Unix.close read_fd;
  (Buffer.contents buf, result)

let resolve_path p = if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let run args =
  let source = Yojson.Safe.Util.(args |> member "source" |> to_string_option) in
  let files =
    match Yojson.Safe.Util.(args |> member "file") with
    | `Null -> []
    | `List l -> List.map Yojson.Safe.Util.to_string l
    | _ -> failwith "\"file\" must be a list of paths"
  in
  let configs =
    match Yojson.Safe.Util.(args |> member "configs") with
    | `Null -> []
    | `List l -> List.map Yojson.Safe.Util.to_string l
    | _ -> failwith "\"configs\" must be a list of paths"
  in
  let mode =
    match Yojson.Safe.Util.(args |> member "mode" |> to_string_option) with
    | None | Some "term" -> Pipeline.Term
    | Some "clauses" -> Pipeline.Clauses
    | Some "commands" -> Pipeline.Commands
    | Some m -> failwith (Printf.sprintf "unknown mode: %s (expected term, clauses, or commands)" m)
  in
  let flag key = Yojson.Safe.Util.(args |> member key |> to_bool_option) |> Option.value ~default:false in
  let display = flag "display" and macros = flag "macros" and doc = flag "doc" and envs = flag "envs" in
  let output = Yojson.Safe.Util.(args |> member "output" |> to_string_option) in
  let sources : (string * string) list =
    match (source, files) with
    | Some s, [] -> [ ("<source>", s) ]
    | Some _, _ :: _ -> failwith "give either \"source\" or \"file\", not both"
    | None, [] -> failwith "give either \"source\" or \"file\""
    | None, fs ->
        List.map
          (fun f ->
            let f = resolve_path f in
            if not (Sys.file_exists f) then failwith (Printf.sprintf "No such file: %s" f);
            (f, read_file f))
          fs
  in
  try
    let cfg = Config.create () in
    List.iter
      (fun c ->
        let c = resolve_path c in
        if not (Sys.file_exists c) then failwith (Printf.sprintf "No such config file: %s" c);
        Config.load_file cfg c)
      configs;
    (* Once every config is in -- entries layer, so a template and the
       macro it calls may come from different files.  Warnings only;
       they reach the client on the trailing line, as Render's do. *)
    Config.check cfg;
    let options : Pipeline.options = { mode; display; macros; doc; envs } in
    let warnings, rendered = capture_stderr (fun () -> Pipeline.render cfg options sources) in
    match output with
    | None ->
        let out = if warnings = "" then rendered else rendered ^ "\n" ^ String.trim warnings ^ "\n" in
        (out, false)
    | Some path ->
        (* Warnings are about the notation, not part of it -- they go in the
           confirmation message, never into the file alongside the TeX. *)
        let path = resolve_path path in
        let oc = open_out path in
        output_string oc rendered;
        close_out oc;
        let msg = Printf.sprintf "Wrote %d bytes to %s." (String.length rendered) path in
        ((if warnings = "" then msg else msg ^ "\n\n" ^ String.trim warnings), false)
  with
  | Lexer.Error m -> failwith ("lexical error: " ^ m)
  | Parser.Error m -> failwith ("parse error: " ^ m)
  | Toplevel.Error m -> failwith m
  | Config.Error m -> failwith ("configuration: " ^ m)
