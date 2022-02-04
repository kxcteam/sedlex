(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

type regexp
and named_slots = {
  begin_var: string;
  end_var: string;
}

type node_action = [`save_offset of save_offset_action]
and save_offset_action = {
  orig : string;
  slot : string;
}

val get_slots: string -> regexp -> string * string
val set_pre_action: node_action -> regexp -> regexp
val set_post_action: node_action -> regexp -> regexp

val chars: Sedlex_cset.t -> regexp
val seq: regexp -> regexp -> regexp
val alt: regexp -> regexp -> regexp
val rep: regexp -> regexp
val plus: regexp -> regexp
val eps: regexp

val compl: regexp -> regexp option
   (* If the argument is a single [chars] regexp, returns a regexp
      which matches the complement set.  Otherwise returns [None]. *)
val subtract: regexp -> regexp -> regexp option
   (* If each argument is a single [chars] regexp, returns a regexp
      which matches the set (arg1 - arg2).  Otherwise returns [None]. *)
val intersection: regexp -> regexp -> regexp option
   (* If each argument is a single [chars] regexp, returns a regexp
      which matches the intersection set.  Otherwise returns [None]. *)

val compile: regexp array -> ((Sedlex_cset.t * int * node_action list) array * bool array) array
