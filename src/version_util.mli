(** This module gives access to the same version/build information returned by
    [Command]-based executables when called with the [-version] or [-build-info] flags
    by [$0 version (-build-info | -version)] or [$0 (-build-info | -version)].
    Here's how it works: we arrange for the build system to, at link time, include an
    object file that defines symbols that version_util.ml uses to get the strings that
    contain the information that this module provides.  When building with OMake, our
    OMakeroot runs build_info.sh to generate *.build_info.c with the symbols and that is
    linked in.
*)

open! Import
open  Std_internal

val version : string (** all hg repos and their versions *)

val version_list : string list (** same as [version], but one string per line *)

val arg_spec : (string * Arg.spec * string) list

(** [Application_specific_fields] is a single field in the build-info sexp that holds
    a [Sexp.t String.Map.t], which can be chosen by the application to hold custom
    fields that it needs. *)
module Application_specific_fields : sig
  type t = Sexp.t String.Map.t [@@deriving sexp]
end

(** various additional information about the circumstances of the build: who built it,
    when, on what machine, etc. *)
val build_info : string
val build_info_as_sexp : Sexp.t

val username                       : string option
val hostname                       : string option
val kernel                         : string option
val build_date                     : Date.t option
val build_time                     : Ofday.t option
val x_library_inlining             : bool
val dynlinkable_code               : bool
val compiled_for_speed             : bool
val application_specific_fields    : Application_specific_fields.t option
val ocaml_version                  : string

val executable_path                : string (** Relative to OMakeroot dir *)

val build_system                   : string
