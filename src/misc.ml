(* *********************************************************************)
(*                                                                     *)
(*                        The ZRun Interpreter                         *)
(*                                                                     *)
(*                             Marc Pouzet                             *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique. All rights reserved. This file is distributed under   *)
(*  the terms of the INRIA Non-Commercial License Agreement (see the   *)
(*  LICENSE file).                                                     *)
(*                                                                     *)
(* *********************************************************************)

(* useful stuff *)
open Unix

exception Error
        
(* version of the compiler *)
let version = "Zrun"
let subversion = "1.0"
let date =
  let { tm_year; tm_wday; tm_yday } = localtime (time ()) in
  (string_of_int tm_year) ^ (string_of_int tm_wday) ^ (string_of_int tm_yday)
  
let header_in_file = version ^ " version " ^ subversion ^ "(" ^ date ^ ")"

(* generic data-structres for sets and symbol tables *)
module S = Set.Make (struct type t = string let compare = compare end)
module Env = Map.Make (struct type t = string let compare = compare end)

(* standard module *)
let name_of_stdlib_module = "Stdlib"

let standard_lib = try Sys.getenv "ZLLIB" with Not_found -> "."

(* list of modules initially opened *)
let default_used_modules = ref [name_of_stdlib_module]

(* load paths *)
let load_path = ref ([standard_lib])

let set_stdlib p =
  load_path := [p]
and add_include d =
  load_path := d :: !load_path;;

(* where is the standard library *)
let locate_stdlib () =
  Printf.printf "%s\n" standard_lib

let show_version () =
  Printf.printf "The %s interpreter, version %s (%s)\n"
    version subversion date;
  Printf.printf "Std lib: "; locate_stdlib ()

let main_node = ref (None: string option)
let set_main s = main_node := Some(s)

let set_check = ref false
              
let number_of_steps = ref 0
let set_number n = number_of_steps := n
 
let no_assert = ref false

let set_verbose = ref false

let set_nocausality = ref false

                        
