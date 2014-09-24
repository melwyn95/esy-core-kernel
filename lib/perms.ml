(* All the types as exposed in the mli are defined in this [Types] module.  The rest of
   this file is simply overriding all the bin_io, compare, and sexp functions to raise
   exceptions. *)
module Types = struct
  module Nobody = struct
    type t with bin_io, compare, sexp
    let name = "Nobody"
  end

  module Me = struct
    type t with bin_io, compare, sexp
    let name = "Me"
  end

  module Read = struct
    type t = [ `Read ]
    with bin_io, compare, sexp
    let name = "Read"
  end

  module Write = struct
    type t = [ `Who_can_write of Me.t ]
    with bin_io, compare, sexp
    let name = "Write"
  end

  module Read_only = struct
    type t = [ Read.t ]
    with bin_io, compare, sexp
    let name = "Read_only"
  end

  module Immutable = struct
    type t = [ Read.t | `Who_can_write of Nobody.t ]
    with bin_io, compare, sexp
    let name = "Immutable"
  end

  module Read_write = struct
    type t = [ Read.t | Write.t ]
    with bin_io, compare, sexp
    let name = "Read_write"
  end

  module Upper_bound = struct
    type 'a t = [ Read.t | `Who_can_write of 'a ]
    with bin_io, compare, sexp
    let name = "Upper_bound"
  end
end

let failwithf = Core_printf.failwithf

(* This is an explicit module type instead of just given inline as the return signature of
   [Only_used_as_phantom_type1] to avoid an unused value warning with bin_io values. *)
module type Sexpable_binable_comparable = sig
  type 'a t = 'a with bin_io, compare, sexp
end

(* Override all bin_io, sexp, compare functions to raise exceptions *)
module Only_used_as_phantom_type1 (Name : sig val name : string end)
  : Sexpable_binable_comparable = struct
  type 'a t = 'a
  let sexp_of_t _ _ =
    failwithf "Unexpectedly called [%s.sexp_of_t]" Name.name ()
  let t_of_sexp _ _ =
    failwithf "Unexpectedly called [%s.t_of_sexp]" Name.name ()
  let compare _ _ _ =
    failwithf "Unexpectedly called [%s.compare]" Name.name ()
  include Bin_prot.Utils.Make_binable1 (struct
    type nonrec 'a t = 'a t
    module Binable = struct
      type 'a t with bin_io
    end
    let to_binable _ =
      failwithf "Unexpectedly used %s bin_io serialization" Name.name ()
    let of_binable _ =
      failwithf "Unexpectedly used %s bin_io deserialization" Name.name ()
  end)
end

module Only_used_as_phantom_type0 (T : sig
  type t with bin_io, compare, sexp
  val name : string
end) : sig
  type t = T.t with bin_io, compare, sexp
  val __t_of_sexp__ : Sexplib.Sexp.t -> t
end = struct
  module M = Only_used_as_phantom_type1 (T)
  type t = T.t M.t with bin_io, compare, sexp
end

module Stable = struct
  module V1 = struct
    module Nobody     = Only_used_as_phantom_type0 (Types.Nobody)
    module Me         = Only_used_as_phantom_type0 (Types.Me)
    module Read       = Only_used_as_phantom_type0 (Types.Read)
    module Write      = Only_used_as_phantom_type0 (Types.Write)
    module Read_write = Only_used_as_phantom_type0 (Types.Read_write)
    module Read_only  = Only_used_as_phantom_type0 (Types.Read_only)
    module Immutable  = Only_used_as_phantom_type0 (Types.Immutable)

    type nobody = Nobody.t with bin_io, compare, sexp
    type me     = Me.t     with bin_io, compare, sexp

    module Upper_bound = struct
      module M = Only_used_as_phantom_type1 (Types.Upper_bound)
      type 'a t = 'a Types.Upper_bound.t M.t with bin_io, compare, sexp
    end
  end

  module Export = struct
    type read_perm        = V1.Read.          t with bin_io, compare, sexp
    type write_perm       = V1.Write.         t with bin_io, compare, sexp
    type read_only_perms  = V1.Read_only.     t with bin_io, compare, sexp
    type immutable_perms  = V1.Immutable.     t with bin_io, compare, sexp
    type read_write_perms = V1.Read_write.    t with bin_io, compare, sexp
    type 'a perms         = 'a V1.Upper_bound.t with bin_io, compare, sexp
  end
end

include Stable.V1
module Export = Stable.Export
