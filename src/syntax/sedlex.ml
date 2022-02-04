(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

module Cset = Sedlex_cset

module StringMap = Map.Make(struct
  type t = string
  let compare = compare
end)

(* NFA *)

type node = {
  id : int;
  mutable action : node_action list;
  mutable eps : node list;
  mutable trans : (Cset.t * node) list;
}
and node_action = [`save_offset of save_offset_action]
and save_offset_action = {
  orig : string;
  slot : string;
}

(* Compilation regexp -> NFA *)

type regexp = {
  nfa : node -> node;
  named_groups : named_slots StringMap.t;
}
and named_slots = {
  begin_var : string;
  end_var : string;
}

let get_slots name re =
  let slots = StringMap.find name re.named_groups in
  (slots.begin_var, slots.end_var)

let set_pre_action act re =
  {re with
   nfa = (fun succ ->
       let init = re.nfa succ in
       init.action <- act :: init.action;
       init)}

let set_post_action act re =
  {re with
   nfa = (fun succ ->
       succ.action <- act :: succ.action;
       re.nfa succ)}

let cur_id = ref 0
let new_node () =
  incr cur_id;
  { id = !cur_id; action = []; eps = []; trans = [] }

let regexp_of_nfa nfa =
  {nfa = nfa;
   named_groups = StringMap.empty;}

let seq r1 r2 =
  regexp_of_nfa (fun succ -> r1.nfa (r2.nfa succ))

let is_chars final = function
  | {eps = []; trans = [c, f]} when f == final -> Some c
  | _ -> None

let chars c = regexp_of_nfa (fun succ ->
    let n = new_node () in
    n.trans <- [c,succ];
    n)

let alt r1 r2 = regexp_of_nfa (fun succ ->
    let nr1 = r1.nfa succ and nr2 = r2.nfa succ in
    match is_chars succ nr1, is_chars succ nr2 with
    | Some c1, Some c2 -> (chars (Cset.union c1 c2)).nfa succ
    | _ ->
      let n = new_node () in
      n.eps <- [r1.nfa succ; r2.nfa succ];
      n)

let rep r = regexp_of_nfa (fun succ ->
    let n = new_node () in
    n.eps <- [r.nfa n; succ];
    n)

let plus r = regexp_of_nfa (fun succ ->
    let n = new_node () in
    let nr = r.nfa n in
    n.eps <- [nr; succ];
    nr)

let eps = regexp_of_nfa (fun succ -> succ) (* eps for epsilon *)

let compl r =
  let n = new_node () in
  match is_chars n (r.nfa n) with
  | Some c ->
    Some (chars (Cset.difference Cset.any c))
  | _ ->
    None

let pair_op f r0 r1 = (* Construct subtract or intersection *)
  let n = new_node () in
  let to_chars r = is_chars n (r.nfa n) in
  match to_chars r0, to_chars r1 with
  | Some c0, Some c1 ->
    Some (chars (f c0 c1))
  | _ ->
    None

let subtract = pair_op Cset.difference

let intersection = pair_op Cset.intersection

let compile_re re =
  let final = new_node () in
  (re.nfa final, final)

(* Determinization *)

type state = node list
      (* A state of the DFA corresponds to a set of nodes in the NFA. *)

let rec add_node state node =
  if List.memq node state then state else add_nodes (node::state) node.eps
and add_nodes state nodes =
  List.fold_left add_node state nodes


let transition (state : state) =
  (* Merge transition with the same target *)
  let rec norm = function
    | (c1, n1)::((c2, n2)::q as l) ->
      if n1 == n2 then norm ((Cset.union c1 c2, n1)::q)
      else (c1, n1)::(norm l)
    | l -> l in
  let t = List.concat (List.map (fun n -> n.trans) state) in
  let t = norm (List.sort (fun (_, n1) (_, n2) -> n1.id - n2.id) t) in

  (* Split char sets so as to make them disjoint *)
  let split (all, t) (c0, n0) =
    let t =
      (Cset.difference c0 all, [n0]) ::
      List.map (fun (c, ns) -> (Cset.intersection c c0, n0::ns)) t @
      List.map (fun (c, ns) -> (Cset.difference c c0, ns)) t
    in
    Cset.union all c0,
    List.filter (fun (c, _) -> not (Cset.is_empty c)) t
  in

  let (_,t) = List.fold_left split (Cset.empty,[]) t in

  (* Epsilon closure of targets *)
  let t = List.map (fun (c, ns) -> (c, add_nodes [] ns)) t in

  (* Canonical ordering *)
  let t = Array.of_list t in
  Array.sort (fun (c1, _) (c2, _) -> compare c1 c2) t;
  t

let compile rs =
  let rs = Array.map compile_re rs in
  let counter = ref 0 in
  let states = Hashtbl.create 31 in
  let states_def = Hashtbl.create 31 in
  let rec aux state =
    try Hashtbl.find states state
    with Not_found ->
      let i = !counter in
      incr counter;
      Hashtbl.add states state i;
      let trans = transition state in
      let trans =
        Array.map
          (fun (p, t) -> (p, aux t, List.concat_map (fun n -> n.action) t))
          trans in
      let finals = Array.map (fun (_, f) -> List.memq f state) rs in
      Hashtbl.add states_def i (trans, finals);
      i
  in
  let init = ref [] in
  Array.iter (fun (i,_) -> init := add_node !init i) rs;
  let i = aux !init in
  assert(i = 0);
  Array.init !counter (Hashtbl.find states_def)
