let getenv_f ~f var =
  try Some (f var) with
  | Not_found -> None
;;

let unsafe_getenv = getenv_f ~f:Unix.unsafe_getenv



open! Core
module Path = Command.Private.Path


module For_unix = Command.Private.For_unix (struct
    module Signal = Signal
    module Thread = Core_thread
    (* module Time = Time_unix *)

    module Unix = struct
      include Core_unix

      let unsafe_getenv = unsafe_getenv
    end

    module Version_util = Version_util
  end)

let run = For_unix.run
let shape = For_unix.shape

module Deprecated = struct
  let run = For_unix.deprecated_run
end

module Shape = struct
  let help_text = For_unix.help_for_shape
end