(** OCaml-Biniou code generator. *)

open Mapping

val make_ocaml_files
  : opens:string list
  -> with_typedefs:bool
  -> with_create:bool
  -> with_fundefs:bool
  -> all_rec:bool
  -> pos_fname:string option
  -> pos_lnum:int option
  -> type_aliases:string option
  -> force_defaults:_ (* not used *)
  -> ocaml_version:(int * int) option
  -> pp_convs:Ocaml.pp_convs
  -> string option -> Ox_emit.target -> unit

val make_ml :
  header:string ->
  opens:string list ->
  with_typedefs:bool ->
  with_create:bool ->
  with_fundefs:bool ->
  original_types:(string, string * int) Hashtbl.t ->
  ocaml_version:(int * int) option ->
  string ->
  ((Ocaml.Repr.t, Biniou.biniou_repr) mapping ->
   (Ocaml.Repr.t, Biniou.biniou_repr) mapping) ->
   (bool * (Ocaml.Repr.t, Biniou.biniou_repr) def list) list ->
  string
