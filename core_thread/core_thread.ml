module Caml_unix = Unix
open! Core
module Unix = Caml_unix

let threads_have_been_created = ref false

include Caml_threads.Thread


let sexp_of_t t = [%message "thread" ~id:(id t : int)]

let create_should_raise = ref false

let create ~on_uncaught_exn f arg =
  if !create_should_raise
  then raise_s [%message "Core_thread.create requested to raise"];
  threads_have_been_created := true;
  let f arg : unit =
    let exit =
      match on_uncaught_exn with
      | `Print_to_stderr -> false
      | `Kill_whole_process -> true
    in
    Exn.handle_uncaught ~exit (fun () -> f arg)
  in
  create f arg
;;

let threads_have_been_created () = !threads_have_been_created

let wait_signal sigs = wait_signal (List.map ~f:Signal.to_caml_int sigs)

let sigmask cmd sigs =
  let cmd =
    match cmd with
    | `Set -> Unix.SIG_SETMASK
    | `Block -> Unix.SIG_BLOCK
    | `Unblock -> Unix.SIG_UNBLOCK
  in
  let sigs = List.map ~f:Signal.to_caml_int sigs in
  List.map ~f:Signal.of_caml_int (sigmask cmd sigs)
;;

let num_threads () =
  let rec find_thread_count = function
    | [] -> None
    | line :: xs ->
      if String.is_prefix line ~prefix:"Threads:" then
        begin
          try
            Some (int_of_string
                    (String.strip (snd (String.lsplit2_exn line ~on:':'))))
          with
          | _ -> None
        end
      else find_thread_count xs
  in
  try
    find_thread_count
      (In_channel.read_lines
         ("/proc/" ^ string_of_int (Unix.getpid ()) ^ "/status"))
  with _ -> None
;;

let block_forever () =
  Event.sync (Event.receive (Event.new_channel ()))
;;