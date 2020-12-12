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

(* global symbol tables *)
open Misc
open Lident
open Global

module E = Map.Make (struct type t = string let compare = compare end)
  
exception Already_defined of string

exception Cannot_find_file of string
  
type env =
    { name: string;
      values: Global.value_desc E.t;
    }
      
type modules =
    { current: env;      (* associated symbol table *)
      opened: env list;  (* opened tables *)
      modules: env E.t;  (* tables loaded in memory *)
    }
      
let current = 
  { name = ""; values = E.empty }
    
let modules = 
  { current = current; opened = []; modules = E.empty }

let findfile filename =
  if Sys.file_exists filename then
    filename
  else if not(Filename.is_implicit filename) then
    raise(Cannot_find_file filename)
  else
    let rec find = function
      [] ->
        raise(Cannot_find_file filename)
    | a::rest ->
        let b = Filename.concat a filename in
          if Sys.file_exists b then b else find rest
    in find !load_path
    
let load_module modname =
  let name = String.uncapitalize_ascii modname in
    try
      let filename = findfile (name ^ ".zci") in
      let ic = open_in_bin filename in
        try
          let m = input_value ic in
            close_in ic;
            m
        with
          | End_of_file | Failure _ ->
              close_in ic;
              Printf.eprintf "Corrupted compiled interface file %s.\n\
                        Please recompile module %s first.\n" filename modname;
              raise Error
    with
      | Cannot_find_file(filename) ->
          Printf.eprintf "Cannot find the compiled interface file %s.\n"
            filename;
          raise Error
            
let find_module ({ modules } as genv) modname =
  try
    E.find modname modules, genv
  with
      Not_found ->
        let m = load_module modname in
        m, { genv with modules = E.add modname m modules }
            
let find genv where qualname =
    let rec findrec ident = function
      | [] -> raise Not_found
      | m :: l ->
          try { qualid = { qual = m.name; id = ident };
                info = where genv ident m }
          with Not_found -> findrec ident l in
      
      match qualname with
        | Modname({ qual = m; id = ident } as q) -> 
           let current, genv =
             if genv.current.name = m then current, genv
             else find_module genv m in
            { qualid = q; info = where genv ident current }
        | Name(ident) -> findrec ident (current :: modules.opened)
            
(* exported functions *)
let open_module genv modname =
  let _, genv = find_module genv modname in
  genv
      
let initialize genv default_used_modules modname = 
  let genv = List.fold_left open_module genv !default_used_modules in
  { genv with current = modname }  
  
let add_value f value = 
  if E.mem f current.values then raise (Already_defined f);
  current.values <- E.add f value current.values
  
let find_value = find (fun ident m -> E.find ident m.values)
  
let write oc = output_value oc current

let qualify n = { qual = current.name; id = n }
let longname n = Modname({ qual = current.name; id = n })
let shortname { id = n } = n
let currentname longname =
  match longname with
    | Name(n) -> longname
    | Modname{ qual = q; id = id} -> 
        if current.name = q then Name(id) else longname
let qualident longname =
  match longname with | Name(n) -> qualify n | Modname(qid) -> qid
let current_module () = current.name
			  
