(* Core_unix wraps the standard unix functions with an exception handler that inserts an
   informative string in the third field of Unix_error.  The problem with the standard
   Unix_error that gets raised is that it doesn't include information about the arguments
   to the function that failed. *)
   (* [%%import "config.h"] *)

   open! Core
   module Unix = UnixLabels
   
   let atom x = Sexp.Atom x
   let list x = Sexp.List x
   
   let record l =
     list (List.map l ~f:(fun (name, value) -> list [atom name; value]))
   ;;
   
   (* No need to include a counter here. It just doesn't make sense to think we are
      going to be receiving a steady stream of interrupts.
      Glibc's macro doesn't have a counter either.
   *)
   let rec retry_until_no_eintr f =
     try
       f ()
     with Unix.Unix_error (EINTR, _, _) ->
       retry_until_no_eintr f
   
   (* This wrapper improves the content of the Unix_error exception raised by the standard
      library (by including a sexp of the function arguments), and it optionally restarts
      syscalls on EINTR. *)
   let improve ?(restart = false) f make_arg_sexps =
     try
       if restart then retry_until_no_eintr f else f ()
     with
     | Unix.Unix_error (e, s, _) ->
       let buf = Buffer.create 100 in
       let fmt = Format.formatter_of_buffer buf in
       Format.pp_set_margin fmt 10000;
       Sexp.pp_hum fmt (record (make_arg_sexps ()));
       Format.pp_print_flush fmt ();
       let arg_str = Buffer.contents buf in
       raise (Unix.Unix_error (e, s, arg_str))
   ;;
   
   module File_descr = struct
     module M = struct
       type t = Unix.file_descr
       external to_int : t -> int = "%identity"
       external of_int : int -> t = "%identity"
       let of_string string = of_int (Int.of_string string)
       let to_string t = Int.to_string (to_int t)
       let hash t = Int.hash (to_int t)
       let compare t1 t2 = Int.compare (to_int t1) (to_int t2)
       let sexp_of_t t =
         (* File descriptors 0, 1, 2 (stdin, stdout, stderr) are stable, so we show them even
            in test. *)
         match am_running_test && Int.(>) (to_int t) 2 with
         | false -> [%sexp (to_int t : int)]
         | true -> [%sexp "_"]
       ;;
     end
     include M
     include (Hashable.Make_plain_and_derive_hash_fold_t (M))
   
     (* Given that [to_int] and [of_int] are set to "%identity", this is considerably more
        direct.  It's unfortunate, but despite [Caml_unix] using [type t = int] in the
        implementation, [Unix.file_descr] is abstract and cannot be tagged [@@immediate]. *)
     let equal (t1 : t) t2 = phys_equal t1 t2
   end
   let fd_r fd = ("fd", File_descr.sexp_of_t fd)


   let in_channel_of_descr = Unix.in_channel_of_descr
   
   let unary ?restart make_r f =
     ();
     fun x -> improve ?restart (fun () -> f x) (fun () -> [make_r x])
   ;;
   
   let unary_fd ?restart f = unary ?restart fd_r f
   
   exception Unix_error = Unix.Unix_error
   
   let putenv ~key ~data =
     improve (fun () -> Unix.putenv key data)
       (fun () -> [("key", atom key); ("data", atom data)])
   ;;
   
   type process_status = Unix.process_status =
     | WEXITED of int
     | WSIGNALED of int
     | WSTOPPED of int
   [@@deriving sexp]
   
   module Exit = struct
     type error = [ `Exit_non_zero of int ] [@@deriving compare, sexp]
   
     type t = (unit, error) Result.t [@@deriving compare, sexp]
   
     let to_string_hum = function
       | Ok () -> "exited normally"
       | Error (`Exit_non_zero i) -> sprintf "exited with code %d" i
     ;;
   
     let code = function
       | Ok () -> 0
       | Error (`Exit_non_zero i) -> i
     ;;
   
     exception Exit_code_must_be_nonnegative of int [@@deriving sexp]
   
     let of_code code =
       if code < 0 then
         raise (Exit_code_must_be_nonnegative code)
       else if code = 0 then
         Ok ()
       else
         Error (`Exit_non_zero code)
     ;;
   
     let or_error = function
       | Ok _ as ok  -> ok
       | Error error -> Or_error.error "Unix.Exit" error sexp_of_error
     ;;
   end
   
   module Exit_or_signal = struct
     type error = [ Exit.error | `Signal of Signal.t ] [@@deriving compare, sexp]
   
     type t = (unit, error) Result.t [@@deriving compare, sexp]
   
     (* let to_string_hum = function
       | Ok () | Error #Exit.error as e -> Exit.to_string_hum e
       | Error (`Signal s) ->
         sprintf "died after receiving %s (signal number %d)"
           (Signal.to_string s) (Signal_unix.to_system_int s)
     ;;
    *)
     exception Of_unix_got_invalid_status of process_status [@@deriving sexp]
   
     let of_unix = function
       | WEXITED i -> if i = 0 then Ok () else Error (`Exit_non_zero i)
       | WSIGNALED i -> Error (`Signal (Signal.of_caml_int i))
       | WSTOPPED _ as status -> raise (Of_unix_got_invalid_status status)
     ;;
   
     let or_error = function
       | Ok _ as ok  -> ok
       | Error error -> Or_error.error "Unix.Exit_or_signal" error sexp_of_error
     ;;
   end
   
   module Exit_or_signal_or_stop = struct
     type error = [ Exit_or_signal.error | `Stop of Signal.t ] [@@deriving sexp]
   
     type t = (unit, error) Result.t [@@deriving sexp]
   
     (* let to_string_hum = function
       | Ok () | Error #Exit_or_signal.error as e -> Exit_or_signal.to_string_hum e
       | Error (`Stop s) ->
         sprintf "stopped by %s (signal number %d)"
           (Signal.to_string s) (Signal_unix.to_system_int s)
     ;; *)
   
     let of_unix = function
       | WEXITED i -> if i = 0 then Ok () else Error (`Exit_non_zero i)
       | WSIGNALED i -> Error (`Signal (Signal.of_caml_int i))
       | WSTOPPED i -> Error (`Stop (Signal.of_caml_int i))
     ;;
   
     let or_error = function
       | Ok _ as ok  -> ok
       | Error error -> Or_error.error "Unix.Exit_or_signal_or_stop" error sexp_of_error
     ;;
   end
   
   module Env = struct
     type t =
       [ `Replace of (string * string) list
       | `Extend of (string * string) list
       | `Override of (string * string option) list
       | `Replace_raw of string list
       ]
     [@@deriving sexp]
   
     let current ~base () =
       let base =
         match base with
         | Some v -> force v
         | None -> Array.to_list (Unix.environment ())
       in
       List.map base ~f:(fun s -> String.lsplit2_exn s ~on:'=')
     ;;
   
     let env_map ~base env =
       let map_of_list list = String.Map.of_alist_reduce list ~f:(fun _ x -> x) in
       match env with
       | `Replace env -> map_of_list env
       | `Extend extend -> map_of_list (current ~base () @ extend)
       | `Override overrides ->
         List.fold_left overrides ~init:(map_of_list (current ~base ()))
           ~f:(fun acc (key, v) ->
             match v with
             | None -> Map.remove acc key
             | Some data -> Map.set acc ~key ~data)
     ;;
   
     let expand ?base env =
       match env with
       | `Replace_raw env -> env
       | `Replace _
       | `Extend  _
       | `Override _ as env ->
         Map.fold (env_map ~base env) ~init:[]
           ~f:(fun ~key ~data acc -> (key ^ "=" ^ data) :: acc)
     ;;
   
     let expand_array ?base env = Array.of_list (expand ?base env)
   end
   
   type env = Env.t [@@deriving sexp]
   let prog_r prog = ("prog", atom prog)
   let args_r argv = ("argv", sexp_of_array atom argv)
   let env_r env = ("env", sexp_of_array atom env)
let execv ~prog ~argv =
  improve (fun () -> Unix.execv ~prog ~args:argv)
    (fun () -> [prog_r prog; args_r argv])
;;

let execve ~prog ~argv ~env =
  improve (fun () -> Unix.execve ~prog ~args:argv ~env)
    (fun () -> [prog_r prog; args_r argv; env_r env])
;;

let execvp ~prog ~argv =
  improve (fun () -> Unix.execvp ~prog ~args:argv)
    (fun () -> [prog_r prog; args_r argv])
;;

let execvpe ~prog ~argv ~env =
  improve (fun () -> Unix.execvpe ~prog ~args:argv ~env)
    (fun () -> [prog_r prog; args_r argv; env_r env])
;;

let exec ~prog ~argv ?(use_path = true) ?env () =
  let argv = Array.of_list argv in
  let env = Option.map env ~f:Env.expand_array in
  match use_path, env with
  | false, None -> execv ~prog ~argv
  | false, Some env -> execve ~prog ~argv ~env
  | true, None -> execvp ~prog ~argv
  | true, Some env -> execvpe ~prog ~argv ~env
;;
   
   type wait_flag =
     Unix.wait_flag =
     | WNOHANG
     | WUNTRACED
   [@@deriving sexp]
   
   type wait_on =
     [ `Any
     | `My_group
     | `Group of Pid.t
     | `Pid of Pid.t
     ]
   [@@deriving sexp]
   
   type mode = wait_flag list [@@deriving sexp_of]
   type _t = mode
   
   type waitpid_result = (Pid.t * Exit_or_signal_or_stop.t) option [@@deriving sexp_of]
   
   let wait_gen
         ~mode
         (type a) (f : waitpid_result -> a option)
         ~restart
         wait_on : a =
     let pid =
       match wait_on with
       | `Any -> -1
       | `Group pid -> - (Pid.to_int pid)
       | `My_group -> 0
       | `Pid pid -> Pid.to_int pid
     in
     let (pid, status) =
       improve ~restart
         (fun () ->
            let x, ps = Unix.waitpid ~mode pid in
            (x, Exit_or_signal_or_stop.of_unix ps))
         (fun () ->
            [("mode", sexp_of_list sexp_of_wait_flag mode);
             ("pid", Int.sexp_of_t pid)])
     in
     let waitpid_result =
       if pid = 0 then
         None
       else begin
         let pid = Pid.of_int pid in
         Some (pid, status)
       end
     in
     match f waitpid_result with
     | Some a -> a
     | None ->
       failwiths ~here:[%here] "waitpid syscall returned invalid result for mode"
         (pid, mode, waitpid_result)
         ([%sexp_of: int * mode * waitpid_result])
   ;;
   
   let wait ?(restart=true) pid =
     let f = function
       | Some ((_, (Ok _ | Error #Exit_or_signal.error)) as x) -> Some x
       | _ -> None
     in
     wait_gen ~restart ~mode:[] f pid
   ;;
   
   let getpid () = Pid.of_int (Unix.getpid ())
   
   let close ?restart = unary_fd ?restart Unix.close
   
   module Process_info = struct
     type t =
       { pid : Pid.t;
         stdin : File_descr.t;
         stdout : File_descr.t;
         stderr : File_descr.t;
       }
     [@@deriving sexp_of]
   end
   
   let create_process_internal
     :  working_dir : string option
       -> prog        : string
       -> argv        : string list
       -> env         : string list
       -> Process_info.t
     =
     fun ~working_dir ~prog ~argv ~env ->
     let close_on_err = ref [] in
     let safe_pipe () =
       let (fd_read, fd_write) as result = Spawn.safe_pipe () in
       close_on_err := fd_read :: fd_write :: !close_on_err;
       result
     in
     try
       let in_read,  in_write  = safe_pipe () in
       let out_read, out_write = safe_pipe () in
       let err_read, err_write = safe_pipe () in
       let pid =
         Spawn.spawn
           ?cwd:(Option.map working_dir ~f:(fun x -> Spawn.Working_dir.Path x))
           ~prog
           ~argv
           ~env:(Spawn.Env.of_list env)
           ~stdin:in_read
           ~stdout:out_write
           ~stderr:err_write
           ()
         |> Pid.of_int
       in
       close in_read; close out_write; close err_write;
       { pid; stdin = in_write; stdout = out_read; stderr = err_read; }
     with exn ->
       List.iter !close_on_err ~f:(fun x -> try close x with _ -> ());
       raise exn
   ;;
   
   module Execvp_emulation : sig
     (* This is a reimplementation of execvp semantics with two main differences:
        - it does [spawn] instead of [execve] and returns its result on success
        - it checks file existence and access rights before trying to spawn.
          This optimization is valuable because a failed [spawn] is much more expensive than a
          failed [execve]. *)
     val run
       :  working_dir : string option
       -> spawn       : (prog:string -> argv:string list -> 'a)
       -> prog        : string
       -> args        : string list
       -> ?prog_search_path : string list
       -> ?argv0      : string
       -> unit
       -> 'a
   
   end = struct
   
     let get_path prog_search_path =
       (match prog_search_path with
        | Some [] -> invalid_arg "Core_unix.create_process: empty prog_search_path"
        | Some dirs -> dirs
        | None -> Sys.getenv "PATH"
                  |> Option.value_map ~f:(String.split ~on:':') ~default:["/bin"; "/usr/bin"]
                  |> List.map ~f:(function
                    | "" -> "."
                    | x  -> x))
     ;;
   
     let candidate_paths ?prog_search_path prog =
       (* [assert] is to make bugs less subtle if we try to make this
          portable to non-POSIX in the future. *)
       assert (String.equal Filename.dir_sep "/");
       if String.contains prog '/' then
         [ prog ]
       else
         List.map (get_path prog_search_path) ~f:(fun h -> h ^/ prog)
     ;;
   
     type 'a spawn1_result =
       | Eaccess           of exn
       | Enoent_or_similar of exn
       | Ok                of 'a
   
     let run ~working_dir ~spawn ~prog ~args ?prog_search_path ?argv0 () =
       let argv = (Option.value argv0 ~default:prog)::args in
       let spawn1 candidate =
         match
           (try
              Unix.access
                (if not (Filename.is_relative candidate) then
                   candidate
                 else
                   match working_dir with
                   | Some working_dir -> working_dir ^/ candidate
                   | None -> candidate)
                ~perm:[Unix.X_OK]
            with Unix_error (code, _, args) ->
              raise (Unix_error (code, "Core_unix.create_process", args)));
           spawn ~prog:candidate ~argv
         with
         | exception Unix_error (ENOEXEC, _, _) -> Ok (
           (* As crazy as it looks, this is what execvp does. It's even documented in the man
              page. *)
           spawn
             ~prog:"/bin/sh"
             ~argv:("/bin/sh" :: candidate :: args))
         | exception (Unix_error (EACCES, _, _) as exn) ->
           Eaccess exn
         | exception (Unix_error (
           (* This list of nonfatal errors comes from glibc and openbsd implementations of
              execvpe, as collected in [execvpe_ml] function in ocaml (see
              otherlibs/unix/unix.ml in https://github.com/ocaml/ocaml/pull/1414). *)
           (EISDIR|ELOOP|ENAMETOOLONG|ENODEV|ENOENT|ENOTDIR|ETIMEDOUT), _, _) as exn) ->
           Enoent_or_similar exn
         | pid -> Ok pid
       in
       let rec go first_eaccess = function
         | [] -> assert false (* [candidate_paths] can't return an empty list *)
         | [ candidate ] ->
           (match spawn1 candidate with
            | Eaccess exn
            | Enoent_or_similar exn ->
              raise (Option.value first_eaccess ~default:exn)
            | Ok pid -> pid)
         | candidate :: (_ :: _ as candidates) ->
           match spawn1 candidate with
           | Eaccess exn ->
             let first_eaccess = Some (Option.value first_eaccess ~default:exn) in
             go first_eaccess candidates
           | Enoent_or_similar _exn ->
             go first_eaccess candidates
           | Ok pid -> pid
       in
       go None (candidate_paths ?prog_search_path prog)
     ;;
   end
   
   let create_process_env ?working_dir ?prog_search_path ?argv0 ~prog ~args ~env () =
     let env_assignments = Env.expand env in
     Execvp_emulation.run
       ~prog
       ~args
       ?argv0
       ?prog_search_path
       ~working_dir
       ~spawn:(fun ~prog ~argv ->
         create_process_internal
           ~working_dir
           ~prog
           ~argv
           ~env:env_assignments)
       ()
   
   let create_process_env ?working_dir ?prog_search_path ?argv0 ~prog ~args ~env () =
     improve (fun () -> create_process_env ?working_dir ?prog_search_path ?argv0 ~prog ~args ~env ())
       (fun () ->
          (match working_dir with
           | None -> []
           | Some working_dir ->
             ["working_dir", atom working_dir]
          ) @
          [("prog", atom prog);
           ("args", sexp_of_list atom args);
           ("env", sexp_of_env env)])