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

(* global data in the symbol tables *)
open Misc
open Ident
open Value
    
type 'a info = { qualid : Lident.qualident; info : 'a }

(* values in the symbol table *)
type value_desc =
    { mutable value: Value.gvalue;
    }

let set_value { info = ({ value = _ } as v)} value = v.value <- value

