(* abella_mcp — an MCP server exposing the Abella proof assistant.
 *
 * Transport: MCP stdio, i.e. newline-delimited JSON-RPC 2.0 on stdin/stdout.
 * stdout carries protocol messages ONLY; all logging goes to stderr.
 *
 * Abella is driven as a subprocess through its interactive REPL over plain
 * pipes.  The REPL protocol is a clean request/response one: write
 * "<command>\n", then read until the output ends with a prompt, which Abella
 * writes as "<name> < " where <name> is "Abella" at the top level and the
 * theorem's name inside a proof.  (No PTY is needed: Abella flushes both its
 * output and its prompt, so a pipe stays in sync.)
 *
 * Loading a file with `abella file.thm -i` processes the file and then drops
 * into interactive mode -- including when the file ends with an incomplete
 * proof, which lands the session exactly at the open subgoal.
 *
 * The abella2tex tool below is unrelated to the Abella subprocess: it links
 * the abella2tex library (an external opam dependency, its own project) to
 * render terms and statements to TeX, the same rendering the standalone
 * abella2tex CLI provides.
 *)

open Abella2tex

let log fmt = Printf.eprintf ("[abella-mcp] " ^^ fmt ^^ "\n%!")

let protocol_version = "2024-11-05"
let server_name = "abella"
let server_version = "0.1.0"

let default_timeout = 60.0

(* ------------------------------------------------------------------ *)
(* Locating the Abella binary                                          *)
(* ------------------------------------------------------------------ *)

let is_executable p =
  try Unix.access p [ Unix.X_OK ]; (Unix.stat p).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error _ -> false

let find_in_path name =
  match Sys.getenv_opt "PATH" with
  | None -> None
  | Some path ->
      String.split_on_char ':' path
      |> List.find_map (fun dir ->
             if dir = "" then None
             else
               let p = Filename.concat dir name in
               if is_executable p then Some p else None)

let abella_bin =
  lazy
    (match Sys.getenv_opt "ABELLA_BIN" with
    | Some p when is_executable p -> p
    | Some p -> failwith (Printf.sprintf "ABELLA_BIN is set to %S, which is not an executable" p)
    | None -> (
        match find_in_path "abella" with
        | Some p -> p
        | None ->
            let home = try Sys.getenv "HOME" with Not_found -> "" in
            let fallback = Filename.concat home ".local/bin/abella" in
            if is_executable fallback then fallback
            else
              failwith
                "Could not find the abella binary: it is not on PATH and \
                 ABELLA_BIN is not set"))

(* ------------------------------------------------------------------ *)
(* Prompt detection                                                    *)
(* ------------------------------------------------------------------ *)

let is_name_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
  || c = '_' || c = '-' || c = '\'' || c = '*'

(* Abella's prompt is "<name> < " at the very end of the output, on its own
   line.  Returns the name, which is "Abella" at the top level and the
   theorem's name inside a proof. *)
let prompt_suffix s =
  let n = String.length s in
  if n < 3 || String.sub s (n - 3) 3 <> " < " then None
  else
    let line_start = match String.rindex_opt s '\n' with Some i -> i + 1 | None -> 0 in
    let name = String.sub s line_start (n - 3 - line_start) in
    if name <> "" && String.for_all is_name_char name then Some name else None

let strip_prompt s =
  match prompt_suffix s with
  | None -> s
  | Some name ->
      let cut = String.length s - String.length name - 3 in
      String.sub s 0 cut

(* ------------------------------------------------------------------ *)
(* Session                                                             *)
(* ------------------------------------------------------------------ *)

type session = {
  pid : int;
  to_abella : Unix.file_descr;
  from_abella : Unix.file_descr;
  file : string option;
  cwd : string;
  mutable prompt : string;   (* "Abella" at top level, else the theorem name *)
  mutable last_state : string;
}

let current : session option ref = ref None

let close_session s =
  (try Unix.close s.to_abella with Unix.Unix_error _ -> ());
  (try Unix.close s.from_abella with Unix.Unix_error _ -> ());
  (try Unix.kill s.pid Sys.sigterm with Unix.Unix_error _ -> ());
  (try ignore (Unix.waitpid [] s.pid) with Unix.Unix_error _ -> ())

let stop_session () =
  match !current with
  | None -> false
  | Some s ->
      close_session s;
      current := None;
      true

type read_result =
  | Prompt of string * string  (* output (prompt stripped), prompt name *)
  | Timed_out of string
  | Eof of string

let read_until_prompt ?(timeout = default_timeout) s =
  let buf = Buffer.create 4096 in
  let bytes = Bytes.create 65536 in
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    let contents = Buffer.contents buf in
    match prompt_suffix contents with
    | Some name -> Prompt (strip_prompt contents, name)
    | None ->
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0. then Timed_out contents
        else
          let ready, _, _ =
            try Unix.select [ s.from_abella ] [] [] (Float.min 0.25 remaining)
            with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
          in
          if ready = [] then loop ()
          else
            let n =
              try Unix.read s.from_abella bytes 0 (Bytes.length bytes)
              with Unix.Unix_error _ -> 0
            in
            if n = 0 then Eof contents
            else (
              Buffer.add_subbytes buf bytes 0 n;
              loop ())
  in
  loop ()

let write_line s line =
  let data = Bytes.of_string (line ^ "\n") in
  let rec go off len =
    if len > 0 then
      let n = Unix.write s.to_abella data off len in
      go (off + n) (len - n)
  in
  go 0 (Bytes.length data)

let start_session ?file ?cwd () =
  ignore (stop_session ());
  let bin = Lazy.force abella_bin in
  let cwd =
    match (cwd, file) with
    | Some d, _ -> d
    | None, Some f -> Filename.dirname (if Filename.is_relative f then Filename.concat (Sys.getcwd ()) f else f)
    | None, None -> Sys.getcwd ()
  in
  (* Abella needs a file argument: reading the development from stdin would
     consume the same channel we need for interaction.  With no file we hand
     it an empty one. *)
  let file_arg, temp =
    match file with
    | Some f -> ((if Filename.is_relative f then Filename.concat (Sys.getcwd ()) f else f), None)
    | None ->
        let tmp = Filename.temp_file "abella_mcp_" ".thm" in
        (tmp, Some tmp)
  in
  if not (Sys.file_exists file_arg) then
    failwith (Printf.sprintf "No such file: %s" file_arg);
  let in_read, in_write = Unix.pipe ~cloexec:false () in
  let out_read, out_write = Unix.pipe ~cloexec:false () in
  let pid =
    let cwd_before = Sys.getcwd () in
    Sys.chdir cwd;
    Fun.protect
      ~finally:(fun () -> Sys.chdir cwd_before)
      (fun () ->
        Unix.create_process bin [| bin; file_arg; "-i" |] in_read out_write out_write)
  in
  Unix.close in_read;
  Unix.close out_write;
  Unix.set_nonblock out_read;
  let s =
    { pid; to_abella = in_write; from_abella = out_read; file; cwd;
      prompt = "Abella"; last_state = "" }
  in
  current := Some s;
  let result = read_until_prompt s in
  Option.iter (fun t -> try Sys.remove t with Sys_error _ -> ()) temp;
  match result with
  | Prompt (out, name) ->
      s.prompt <- name;
      s.last_state <- out;
      (s, out)
  | Timed_out out ->
      ignore (stop_session ());
      failwith ("Abella did not become ready within the timeout. Output so far:\n" ^ out)
  | Eof out ->
      ignore (stop_session ());
      failwith ("Abella exited while starting up. Output:\n" ^ out)

let require_session () =
  match !current with
  | Some s -> s
  | None ->
      failwith
        "No Abella session is running. Use abella_start first (optionally with \
         a file to load)."

(* ------------------------------------------------------------------ *)
(* Interpreting Abella's replies                                       *)
(* ------------------------------------------------------------------ *)

let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
  nl = 0 || go 0

(* Abella reports failures as text on an otherwise normal reply, so errors are
   detected by inspecting the output rather than by an exit status. *)
let output_has_error out =
  contains ~needle:"Error:" out
  || contains ~needle:"Syntax error" out
  || contains ~needle:"Typing error" out

let trim s = String.trim s

let lines_of s = String.split_on_char '\n' s

let is_diagnostic l =
  contains ~needle:"Error:" l || contains ~needle:"Syntax error" l
  || contains ~needle:"Typing error" l || contains ~needle:"Proof NOT Completed" l

let diagnostics out =
  lines_of out |> List.map trim |> List.filter (fun l -> l <> "" && is_diagnostic l)

(* Everything Abella printed after switching to interactive mode: when a file
   is loaded, this is the part that matters (the state it landed in), as
   opposed to the echo of every command in the file. *)
let marker = "Switching to interactive mode."

let after_marker out =
  let n = String.length out and m = String.length marker in
  let rec find i = if i + m > n then None else if String.sub out i m = marker then Some (i + m) else find (i + 1) in
  match find 0 with None -> None | Some i -> Some (String.sub out i (n - i))

(* The lines around the first diagnostic, which is where a failure actually
   happened; the rest of a long transcript is noise. *)
let error_context ?(before = 12) ?(after = 6) out =
  let ls = lines_of (trim out) in
  let n = List.length ls in
  match List.find_index is_diagnostic ls with
  | None ->
      let keep = 25 in
      if n <= keep then trim out
      else String.concat "\n" (List.filteri (fun i _ -> i >= n - keep) ls)
  | Some i ->
      let lo = max 0 (i - before) and hi = min (n - 1) (i + after) in
      let body = List.filteri (fun j _ -> j >= lo && j <= hi) ls |> String.concat "\n" in
      (if lo > 0 then "... (earlier output elided) ...\n" else "")
      ^ body
      ^ if hi < n - 1 then "\n... (later output elided) ..." else ""

(* ------------------------------------------------------------------ *)
(* Tools                                                               *)
(* ------------------------------------------------------------------ *)

(* Split a blob of Abella source into individual commands.  Commands are
   terminated by '.', but periods also occur inside "%" comments, and Abella's
   own lexer is the only real authority; this handles comments and the common
   cases, which is what a REPL driver needs. *)
let split_commands src =
  let cmds = ref [] in
  let buf = Buffer.create 64 in
  let n = String.length src in
  let flush () =
    let c = trim (Buffer.contents buf) in
    if c <> "" then cmds := c :: !cmds;
    Buffer.clear buf
  in
  let i = ref 0 in
  while !i < n do
    let c = src.[!i] in
    if c = '%' then begin
      (* line comment: skip to end of line *)
      while !i < n && src.[!i] <> '\n' do incr i done
    end
    else if c = '/' && !i + 1 < n && src.[!i + 1] = '*' then begin
      (* block comment *)
      i := !i + 2;
      let fin = ref false in
      while (not !fin) && !i < n do
        if src.[!i] = '*' && !i + 1 < n && src.[!i + 1] = '/' then (i := !i + 2; fin := true)
        else incr i
      done
    end
    else begin
      Buffer.add_char buf c;
      if c = '.' then begin
        (* A '.' ends a command unless it is part of a float or a qualified
           name; Abella has neither at command level. *)
        flush ()
      end;
      incr i
    end
  done;
  flush ();
  List.rev !cmds

let send_commands ?(timeout = default_timeout) s cmds =
  let transcript = Buffer.create 256 in
  let error = ref None in
  let rec go = function
    | [] -> ()
    | cmd :: rest -> (
        write_line s cmd;
        match read_until_prompt ~timeout s with
        | Prompt (out, name) ->
            s.prompt <- name;
            s.last_state <- out;
            Buffer.add_string transcript (Printf.sprintf "> %s\n%s" cmd (trim out));
            Buffer.add_char transcript '\n';
            if output_has_error out then error := Some (`Cmd cmd)
            else go rest
        | Timed_out out ->
            ignore (stop_session ());
            Buffer.add_string transcript
              (Printf.sprintf "> %s\n%s\n" cmd (trim out));
            error := Some (`Timeout (cmd, timeout))
        | Eof out ->
            ignore (stop_session ());
            Buffer.add_string transcript (Printf.sprintf "> %s\n%s\n" cmd (trim out));
            error := Some `Exited)
  in
  go cmds;
  (Buffer.contents transcript, !error)

let tool_start args =
  let file = Yojson.Safe.Util.(args |> member "file" |> to_string_option) in
  let cwd = Yojson.Safe.Util.(args |> member "cwd" |> to_string_option) in
  let s, out = start_session ?file ?cwd () in
  let header =
    match file with
    | Some f -> Printf.sprintf "Started Abella session on %s." (Filename.basename f)
    | None -> "Started an empty Abella session."
  in
  (* Loading a file echoes every command in it; report the state it landed in
     and any diagnostics, not the echo. *)
  let state = match after_marker out with Some tail -> trim tail | None -> trim out in
  let diags = diagnostics out in
  let body =
    if diags = [] then
      if state = "" then "Loaded cleanly; at the top level with no open proof."
      else "Loaded. Current state:\n\n" ^ state
    else
      Printf.sprintf
        "Loading reported:\n  %s\n\nThe session is parked at that point. Context:\n\n%s%s"
        (String.concat "\n  " diags)
        (error_context out)
        (if state = "" then "" else "\n\nCurrent state:\n\n" ^ state)
  in
  s.last_state <- state;
  (Printf.sprintf "%s\n\n%s\n\n[prompt: %s]" header body s.prompt, diags <> [])

let tool_send args =
  let s = require_session () in
  let commands = Yojson.Safe.Util.(args |> member "commands" |> to_string) in
  let timeout =
    match Yojson.Safe.Util.(args |> member "timeout_seconds" |> to_number_option) with
    | Some t -> t
    | None -> default_timeout
  in
  let cmds = split_commands commands in
  if cmds = [] then ("No commands to send.", false)
  else
    let transcript, error = send_commands ~timeout s cmds in
    match error with
    | None ->
        let prompt = match !current with Some s -> s.prompt | None -> "?" in
        (trim transcript ^ Printf.sprintf "\n\n[prompt: %s]" prompt, false)
    | Some (`Cmd cmd) ->
        ( trim transcript
          ^ Printf.sprintf "\n\nStopped: %S reported an error; remaining commands were not sent." cmd,
          true )
    | Some (`Timeout (cmd, t)) ->
        ( trim transcript
          ^ Printf.sprintf
              "\n\nTimed out after %gs on %S. The session was killed to avoid \
               desynchronisation; restart with abella_start."
              t cmd,
          true )
    | Some `Exited ->
        (trim transcript ^ "\n\nAbella exited. Restart with abella_start.", true)

let tool_state _args =
  let s = require_session () in
  let body = if trim s.last_state = "" then "(no proof state; at top level)" else trim s.last_state in
  let loaded = match s.file with Some f -> f | None -> "(none)" in
  ( Printf.sprintf "%s\n\n[file: %s | cwd: %s | prompt: %s]" body loaded s.cwd s.prompt,
    false )

let tool_undo args =
  let s = require_session () in
  let count =
    match Yojson.Safe.Util.(args |> member "count" |> to_int_option) with
    | Some n when n > 0 -> n
    | _ -> 1
  in
  let cmds = List.init count (fun _ -> "undo.") in
  let transcript, error = send_commands s cmds in
  match error with
  | None -> (trim transcript ^ Printf.sprintf "\n\n[prompt: %s]" s.prompt, false)
  | Some _ -> (trim transcript, true)

let tool_stop _args =
  if stop_session () then ("Abella session stopped.", false)
  else ("No session was running.", false)

(* Batch check: run Abella non-interactively over a file and report the
   outcome.  Abella signals failure through its exit status here. *)
let tool_check args =
  let file = Yojson.Safe.Util.(args |> member "file" |> to_string) in
  let file = if Filename.is_relative file then Filename.concat (Sys.getcwd ()) file else file in
  if not (Sys.file_exists file) then failwith (Printf.sprintf "No such file: %s" file);
  let bin = Lazy.force abella_bin in
  let out_read, out_write = Unix.pipe ~cloexec:false () in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let cwd_before = Sys.getcwd () in
  Sys.chdir (Filename.dirname file);
  let pid =
    Fun.protect
      ~finally:(fun () -> Sys.chdir cwd_before)
      (fun () -> Unix.create_process bin [| bin; file |] devnull out_write out_write)
  in
  Unix.close out_write;
  Unix.close devnull;
  let buf = Buffer.create 4096 in
  let bytes = Bytes.create 65536 in
  let rec drain () =
    let n = try Unix.read out_read bytes 0 (Bytes.length bytes) with Unix.Unix_error _ -> 0 in
    if n > 0 then (Buffer.add_subbytes buf bytes 0 n; drain ())
  in
  drain ();
  Unix.close out_read;
  let _, status = Unix.waitpid [] pid in
  let out = Buffer.contents buf in
  let diags = diagnostics out in
  let ok = status = Unix.WEXITED 0 && diags = [] in
  let name = Filename.basename file in
  if ok then
    let completed =
      lines_of out |> List.filter (fun l -> contains ~needle:"Proof completed" l) |> List.length
    in
    (Printf.sprintf "%s: OK -- %d proof(s) completed." name completed, false)
  else
    ( Printf.sprintf "%s: FAILED.\n\n%s\n\nContext:\n\n%s" name
        (String.concat "\n" (List.map (fun d -> "  " ^ d) diags))
        (error_context out),
      true )

(* ------------------------------------------------------------------ *)
(* abella2tex: rendering Abella terms and statements to TeX            *)
(* ------------------------------------------------------------------ *)

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

(* A FILE's own name, stripped to a bare title for \section{}: no
   directory, no extension, and TeX-escaped (an underscore in a
   filename is common and not valid TeX on its own). Mirrors the
   abella2tex CLI's own section_title, in its own repository. *)
let section_title (path : string) : string =
  Render.escape (Filename.remove_extension (Filename.basename path))

let tool_abella2tex args =
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
    | None | Some "term" -> `Term
    | Some "clauses" -> `Clauses
    | Some "commands" -> `Commands
    | Some m -> failwith (Printf.sprintf "unknown mode: %s (expected term, clauses, or commands)" m)
  in
  let flag key = Yojson.Safe.Util.(args |> member key |> to_bool_option) |> Option.value ~default:false in
  let display = flag "display" and macros = flag "macros" and doc = flag "doc" and envs = flag "envs" in
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
    let render_one source_text =
      match mode with
      | `Commands ->
          let cmds = Toplevel.commands source_text in
          let cmds = if envs then Toplevel.group_kinds cmds else cmds in
          cmds
          |> List.filter_map (Render.command ~envs cfg)
          |> List.map (fun s -> s ^ "\n")
          |> String.concat "\n"
      | `Clauses ->
          let cs = Parser.clauses source_text in
          let n = List.length cs in
          let b = Buffer.create 256 in
          Buffer.add_string b "\\begin{align*}\n";
          List.iteri
            (fun i c ->
              let sep = if i = n - 1 then "" else "\\\\" in
              Buffer.add_string b ("  &" ^ Render.clause cfg c ^ sep ^ "\n"))
            cs;
          Buffer.add_string b "\\end{align*}\n";
          Buffer.contents b
      | `Term ->
          let s = Render.term cfg (Parser.term source_text) in
          if display then "\\[\n  " ^ s ^ "\n\\]\n" else s ^ "\n"
    in
    let source_name =
      match sources with
      | [ (name, _) ] -> name
      | many -> String.concat ", " (List.map fst many)
    in
    let warnings, body =
      capture_stderr (fun () ->
          match sources with
          | [ (_, source_text) ] -> render_one source_text
          | many ->
              many
              |> List.map (fun (name, source_text) ->
                     "\\section{" ^ section_title name ^ "}\n\n" ^ render_one source_text)
              |> String.concat "\n\n")
    in
    let out =
      if doc then
        let envs_packages = if envs then "\\usepackage{amsthm,mathpartir,xcolor}\n" else "" in
        let macro_packages = "\\usepackage{mathtools,graphicx,mathrsfs}\n" in
        let envs_theorems =
          if envs then
            "\\newtheorem{theorem}{Theorem}\n\\theoremstyle{definition}\n\\newtheorem{definition}{Definition}\n\n"
          else ""
        in
        "% generated by abella2tex --doc from " ^ source_name ^ "\n"
        ^ "\\documentclass[11pt]{article}\n\\usepackage{amsmath,amssymb}\n"
        ^ macro_packages ^ envs_packages ^ "\n" ^ Render.macros cfg ^ "\n\n" ^ envs_theorems
        ^ "\\begin{document}\n\n" ^ body ^ "\n\\end{document}\n"
      else if macros then Render.macros cfg ^ "\n\n" ^ body
      else body
    in
    let out = if warnings = "" then out else out ^ "\n" ^ String.trim warnings ^ "\n" in
    (out, false)
  with
  | Lexer.Error m -> failwith ("lexical error: " ^ m)
  | Parser.Error m -> failwith ("parse error: " ^ m)
  | Toplevel.Error m -> failwith m
  | Config.Error m -> failwith ("configuration: " ^ m)

(* ------------------------------------------------------------------ *)
(* Tool registry                                                       *)
(* ------------------------------------------------------------------ *)

type tool = {
  name : string;
  description : string;
  schema : Yojson.Safe.t;
  run : Yojson.Safe.t -> string * bool;  (* text, is_error *)
}

let obj props required =
  `Assoc
    [ ("type", `String "object");
      ("properties", `Assoc props);
      ("required", `List (List.map (fun r -> `String r) required)) ]

let str_prop desc = `Assoc [ ("type", `String "string"); ("description", `String desc) ]
let int_prop desc = `Assoc [ ("type", `String "integer"); ("description", `String desc) ]
let num_prop desc = `Assoc [ ("type", `String "number"); ("description", `String desc) ]
let bool_prop desc = `Assoc [ ("type", `String "boolean"); ("description", `String desc) ]

let str_array_prop desc =
  `Assoc
    [ ("type", `String "array");
      ("items", `Assoc [ ("type", `String "string") ]);
      ("description", `String desc) ]

let enum_prop values desc =
  `Assoc
    [ ("type", `String "string");
      ("enum", `List (List.map (fun s -> `String s) values));
      ("description", `String desc) ]

let tools =
  [
    { name = "abella_check";
      description =
        "Batch-check an Abella .thm file end to end and report whether every \
         proof completed. Use this to verify a file after editing it. Returns \
         the tail of Abella's output, where any error appears.";
      schema = obj [ ("file", str_prop "Path to the .thm file to check.") ] [ "file" ];
      run = tool_check };
    { name = "abella_start";
      description =
        "Start (or restart) an interactive Abella session. If a file is given \
         it is loaded first, and the session is left wherever the file ends -- \
         including at an open subgoal if the file's last proof is incomplete, \
         which is the usual way to pick up work in progress. Replaces any \
         existing session.";
      schema =
        obj
          [ ("file", str_prop "Optional .thm file to load before going interactive.");
            ("cwd", str_prop
               "Working directory for resolving Specification/Import. Defaults \
                to the file's directory.") ]
          [];
      run = tool_start };
    { name = "abella_send";
      description =
        "Send one or more commands to the running Abella session and return \
         the transcript with the resulting proof state. Commands are ordinary \
         Abella syntax terminated by '.', e.g. 'intros. case H1. search.'; \
         both top-level commands (Kind, Type, Define, Theorem) and proof \
         tactics are accepted. Stops at the first command that errors, so \
         earlier commands' effects still stand.";
      schema =
        obj
          [ ("commands", str_prop
               "Abella commands, each terminated by '.'. Comments are allowed.");
            ("timeout_seconds", num_prop
               "Per-command timeout. Defaults to 60. Raise it for a deep \
                'search'.") ]
          [ "commands" ];
      run = tool_send };
    { name = "abella_state";
      description =
        "Show the current proof state of the running session (the goal, \
         hypotheses and any remaining subgoals) without changing it.";
      schema = obj [] [];
      run = tool_state };
    { name = "abella_undo";
      description =
        "Undo the last proof command(s) in the running session. Only \
         meaningful inside a proof.";
      schema = obj [ ("count", int_prop "How many commands to undo. Defaults to 1.") ] [];
      run = tool_undo };
    { name = "abella_stop";
      description = "Stop the running Abella session and release the subprocess.";
      schema = obj [] [];
      run = tool_stop };
    { name = "abella2tex";
      description =
        "Render an Abella term, a ';'-separated list of Define clauses, or a \
         whole .thm file's top-level declarations to TeX, under a notation \
         configuration (see notation.conf's header comment for the format). \
         Give either \"source\" (raw Abella text -- a leading 'Theorem NAME :' \
         or 'Define ... by' header and a trailing '.' are stripped, so it can \
         be pasted straight from a .thm file) or \"file\" (paths to read \
         instead). With more than one file, each is rendered independently \
         and wrapped in its own \\section{FILE}, in the order given, matching \
         the abella2tex CLI. Variable-naming collisions the rendering would \
         introduce are reported on a trailing line.";
      schema =
        obj
          [ ("source", str_prop "Abella source text to render.");
            ("file", str_array_prop
               "Path(s) of file(s) to render instead of \"source\". With more \
                than one, each file is rendered independently and wrapped in \
                its own \\section{FILE}, in the order given.");
            ("configs", str_array_prop
               "Notation configuration file paths, applied in order (later \
                files refine earlier ones).");
            ("mode", enum_prop [ "term"; "clauses"; "commands" ]
               "\"term\" (default) renders a single term or Theorem \
                statement; \"clauses\" reads ';'-separated Define clauses as \
                an align* block; \"commands\" reads a whole .thm file and \
                renders each Kind, Type, Define/CoDefine and Theorem in \
                turn (proofs are dropped).");
            ("display", bool_prop "In \"term\" mode, wrap the result in \\[ ... \\].");
            ("macros", bool_prop
               "Emit \\newcommand for every macro the configuration(s) \
                declare, before the rendering.");
            ("doc", bool_prop
               "Wrap the output in a complete, compilable .tex document \
                (\\documentclass, the macros, \\begin{document} ...). \
                Implies \"macros\".");
            ("envs", bool_prop
               "With mode=\"commands\": chunk Kinds with their constructors \
                into one mathpar, wrap each Define in a definition \
                environment (or one \\inferrule per clause if configured \
                \"rule=true\") and each Theorem in a theorem environment.") ]
          [];
      run = tool_abella2tex };
  ]

let tool_json t =
  `Assoc
    [ ("name", `String t.name);
      ("description", `String t.description);
      ("inputSchema", t.schema) ]

(* ------------------------------------------------------------------ *)
(* JSON-RPC 2.0, via the jsonrpc package                               *)
(* ------------------------------------------------------------------ *)

module Rpc = Jsonrpc

(* Request params arrive as an optional structured value; the tool
   argument extractors below want a plain JSON object. *)
let params_json (params : Rpc.Structured.t option) : Yojson.Safe.t =
  match params with None -> `Assoc [] | Some s -> (s :> Yojson.Safe.t)

let send_response (resp : Rpc.Response.t) =
  print_string (Yojson.Safe.to_string (Rpc.Response.yojson_of_t resp));
  print_newline ();
  flush stdout

let ok id result : Rpc.Response.t = Rpc.Response.ok id result

let error id code msg : Rpc.Response.t =
  Rpc.Response.error id (Rpc.Response.Error.make ~code ~message:msg ())

let text_result text is_error =
  `Assoc
    [ ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ]);
      ("isError", `Bool is_error) ]

let handle_initialize id params : Rpc.Response.t =
  let requested =
    Yojson.Safe.Util.(params |> member "protocolVersion" |> to_string_option)
  in
  let version = match requested with Some v -> v | None -> protocol_version in
  ok id
    (`Assoc
      [ ("protocolVersion", `String version);
        ("capabilities", `Assoc [ ("tools", `Assoc []) ]);
        ("serverInfo", `Assoc [ ("name", `String server_name); ("version", `String server_version) ]) ])

let handle_tools_call id params : Rpc.Response.t =
  let name = Yojson.Safe.Util.(params |> member "name" |> to_string) in
  let args =
    match Yojson.Safe.Util.(params |> member "arguments") with
    | `Null -> `Assoc []
    | a -> a
  in
  match List.find_opt (fun t -> t.name = name) tools with
  | None ->
      error id Rpc.Response.Error.Code.InvalidParams
        (Printf.sprintf "Unknown tool: %s" name)
  | Some t -> (
      match t.run args with
      | text, is_error -> ok id (text_result text is_error)
      | exception Failure msg -> ok id (text_result ("Error: " ^ msg) true)
      | exception Yojson.Safe.Util.Type_error (msg, _) ->
          ok id (text_result ("Bad arguments: " ^ msg) true)
      | exception e -> ok id (text_result ("Error: " ^ Printexc.to_string e) true))

let handle_request (req : Rpc.Request.t) : Rpc.Response.t =
  let { Rpc.Request.id; method_; params } = req in
  let params = params_json params in
  match method_ with
  | "initialize" -> handle_initialize id params
  | "ping" -> ok id (`Assoc [])
  | "tools/list" ->
      ok id (`Assoc [ ("tools", `List (List.map tool_json tools)) ])
  | "tools/call" -> handle_tools_call id params
  | m ->
      error id Rpc.Response.Error.Code.MethodNotFound
        (Printf.sprintf "Method not found: %s" m)

(* The server only ever receives requests and notifications: it answers
   requests and ignores notifications (and, defensively, anything else a
   client should never send). *)
let handle_packet (packet : Rpc.Packet.t) =
  match packet with
  | Rpc.Packet.Request req -> send_response (handle_request req)
  | Rpc.Packet.Notification _ -> ()
  | Rpc.Packet.Batch_call calls ->
      List.iter
        (function
          | `Request req -> send_response (handle_request req)
          | `Notification _ -> ())
        calls
  | Rpc.Packet.Response _ | Rpc.Packet.Batch_response _ -> ()

let main () =
  (* A dead Abella subprocess must not take the server down with it. *)
  (try ignore (Sys.signal Sys.sigpipe Sys.Signal_ignore) with Invalid_argument _ -> ());
  at_exit (fun () -> ignore (stop_session ()));
  (* [at_exit] only runs on a graceful return from [loop] (stdin EOF); an MCP
     client tearing this process down with a signal instead -- the usual way
     to stop a stdio server -- would hit OCaml's default disposition, which
     terminates immediately and skips [at_exit], orphaning the Abella child
     with no server left that knows its pid to ever kill it again. Routing
     these through [exit] instead makes the shutdown "graceful" either way. *)
  let handle_shutdown_signal _ = exit 0 in
  List.iter
    (fun sg -> try Sys.set_signal sg (Sys.Signal_handle handle_shutdown_signal)
      with Invalid_argument _ -> ())
    [ Sys.sigterm; Sys.sigint ];
  log "started (protocol %s)" protocol_version;
  let rec loop () =
    match input_line stdin with
    | exception End_of_file -> ()
    | line ->
        let line = String.trim line in
        if line <> "" then (
          match Yojson.Safe.from_string line with
          | json -> (
              match Rpc.Packet.t_of_yojson json with
              | packet -> (
                  try handle_packet packet
                  with e -> log "handler error: %s" (Printexc.to_string e))
              | exception e -> log "bad JSON-RPC packet: %s" (Printexc.to_string e))
          | exception Yojson.Json_error e -> log "malformed JSON: %s" e);
        loop ()
  in
  loop ()

let () = main ()
