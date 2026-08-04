
open Parsing

(* ------------------------------------------------------------------ *)
(* Proof states, and the delta between two of them                     *)
(* ------------------------------------------------------------------ *)

(* Abella reprints the whole proof state after every tactic: the induction
   hypothesis, every unchanged hypothesis, the goal, and the statement of each
   pending subgoal.  Over a long proof that is the same text again and again,
   so abella_send reports only what a command actually changed and leaves the
   full picture to abella_state.

   A state block looks like

     Subgoal 1.1:                 (only inside a multi-subgoal proof)

     Variables: BC ABC            (only when there are eigenvariables)
     IH : forall A B C, nat A * ->
            add A B AB            (continuation lines are indented)
     H3 : add z ABC BC
     ============================
      add z BC ABC

     Subgoal 1.2 is:              (pending subgoals: statement only)
      add z BC (s K)
*)

type pstate = {
  messages : string list;         (* lines above the block, e.g. "Error: ..." *)
  header : string;                (* "Subgoal 1.1:", or "" outside a split *)
  variables : string;             (* the "Variables: ..." line, or "" *)
  hyps : (string * string) list;  (* name, text as printed *)
  goal : string;
  pending : string list;          (* the "Subgoal N is:" headers *)
}

let empty_state =
  { messages = []; header = ""; variables = ""; hyps = []; goal = ""; pending = [] }

(* Wrapping depends on the width of the "NAME : " prefix, so the same formula
   can be laid out differently under H1 and under H12; compare on collapsed
   whitespace so that re-wrapping alone never reads as a change. *)
let normalize s =
  let b = Buffer.create (String.length s) in
  let gap = ref true in
  String.iter
    (fun c ->
      if c = ' ' || c = '\t' || c = '\n' || c = '\r' then
        if not !gap then (Buffer.add_char b ' '; gap := true) else ()
      else (Buffer.add_char b c; gap := false))
    s;
  trim (Buffer.contents b)

let is_separator l =
  let l = trim l in
  l <> "" && String.for_all (fun c -> c = '=') l

let is_indented l = l <> "" && (l.[0] = ' ' || l.[0] = '\t')

(* "H1 : nat A @" opens a hypothesis; "Variables: A B" and "Subgoal 1:" do not,
   because the separator Abella prints is " : ", spaces included. *)
let hyp_name l =
  if is_indented l then None
  else
    match String.index_opt l ':' with
    | None -> None
    | Some i ->
        if i = 0 || i + 1 >= String.length l then None
        else if l.[i - 1] <> ' ' || l.[i + 1] <> ' ' then None
        else
          let n = trim (String.sub l 0 (i - 1)) in
          if n <> "" && String.for_all is_name_char n then Some n else None

let is_subgoal_header l =
  let l = trim l in
  String.starts_with ~prefix:"Subgoal " l
  && String.ends_with ~suffix:":" l
  && not (String.ends_with ~suffix:" is:" l)

let is_pending_header l =
  let l = trim l in
  String.starts_with ~prefix:"Subgoal " l && String.ends_with ~suffix:" is:" l

(* [List.find_index] finds the first match; we want the last, so scan the
   reversed list and translate the index back. *)
let last_index_of p ls =
  let n = List.length ls in
  Option.map (fun i -> n - 1 - i) (List.find_index p (List.rev ls))

let parse_above ls =
  let messages = ref [] and header = ref "" and variables = ref "" in
  let hyps = ref [] and cur = ref None in
  let flush () =
    match !cur with
    | Some (n, buf) ->
        hyps := (n, String.concat "\n" (List.rev buf)) :: !hyps;
        cur := None
    | None -> ()
  in
  List.iter
    (fun l ->
      match hyp_name l with
      | Some n -> flush (); cur := Some (n, [ l ])
      | None ->
          if trim l = "" then ()
          else if is_subgoal_header l then (flush (); header := trim l)
          else if String.starts_with ~prefix:"Variables:" (trim l) then
            (flush (); variables := trim l)
          else (
            match !cur with
            | Some (n, buf) when is_indented l -> cur := Some (n, l :: buf)
            | _ -> flush (); messages := trim l :: !messages))
    ls;
  flush ();
  (List.rev !messages, !header, !variables, List.rev !hyps)

let parse_below ls =
  let rec split acc = function
    | [] -> (List.rev acc, [])
    | l :: rest when is_pending_header l -> (List.rev acc, l :: rest)
    | l :: rest -> split (l :: acc) rest
  in
  let goal_ls, pend_ls = split [] ls in
  (trim (String.concat "\n" goal_ls), List.filter_map
     (fun l -> if is_pending_header l then Some (trim l) else None) pend_ls)

(* [None] when the reply carries no proof state at all: a top-level command, a
   "Proof completed", a bare error. *)
let parse_state out =
  let ls = lines_of out in
  match last_index_of is_separator ls with
  | None -> None
  | Some sep ->
      let above = List.filteri (fun i _ -> i < sep) ls in
      let below = List.filteri (fun i _ -> i > sep) ls in
      let messages, header, variables, hyps = parse_above above in
      let goal, pending = parse_below below in
      Some { messages; header; variables; hyps; goal; pending }

let render_delta ~prev ~curr =
  let out = Buffer.create 256 in
  let add s = Buffer.add_string out s; Buffer.add_char out '\n' in
  let changed =
    List.filter
      (fun (n, t) ->
        match List.assoc_opt n prev.hyps with
        | Some t' -> normalize t <> normalize t'
        | None -> true)
      curr.hyps
  in
  let unchanged =
    List.filter_map
      (fun (n, t) ->
        match List.assoc_opt n prev.hyps with
        | Some t' when normalize t = normalize t' -> Some n
        | _ -> None)
      curr.hyps
  in
  let removed =
    List.filter_map
      (fun (n, _) -> if List.mem_assoc n curr.hyps then None else Some n)
      prev.hyps
  in
  let new_header = curr.header <> "" && curr.header <> prev.header in
  let new_vars = curr.variables <> "" && normalize curr.variables <> normalize prev.variables in
  let new_goal = normalize curr.goal <> normalize prev.goal in
  let np = List.length curr.pending and np_prev = List.length prev.pending in
  let quiet =
    curr.messages = [] && (not new_header) && (not new_vars) && (not new_goal)
    && changed = [] && removed = [] && np = np_prev
  in
  if quiet then add "(state unchanged)"
  else begin
    List.iter add curr.messages;
    if new_header then add curr.header;
    if new_vars then add curr.variables;
    if removed <> [] then add ("- " ^ String.concat ", " removed);
    List.iter (fun (_, t) -> add t) changed;
    if unchanged <> [] then add ("(unchanged: " ^ String.concat ", " unchanged ^ ")");
    add (if new_goal then "goal: " ^ curr.goal else "goal: (unchanged)");
    if np > 0 then
      add (Printf.sprintf "(%d other subgoal%s pending)" np (if np = 1 then "" else "s"))
    else if np_prev > 0 then add "(no other subgoals pending)"
  end;
  String.trim (Buffer.contents out)

(* What to show for one command: the change if we can read a proof state out of
   the reply, the reply itself otherwise. *)
let summarize ~prev ~curr =
  Option.bind (parse_state curr) (fun c ->
      let p = Option.value ~default:empty_state (parse_state prev) in
      Some (render_delta ~prev:p ~curr:c))
  |> Option.value ~default:(trim curr)
