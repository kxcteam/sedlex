(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

type regexp

type node_action = [`save_offset of save_offset_action]
and save_offset_action =
  | Save_offset_assign of {
      varname : string;
    }
  | Save_offset_update of {
      varname : string;
      update_function : prev:(int option) -> curr:int -> int option
    }

val get_names: regexp -> string list
val get_slots: string -> regexp -> string * string
val set_slots: string -> string * string -> regexp -> regexp
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

val compile: regexp array -> ((Sedlex_cset.t * int) array * bool array * node_action list) array
