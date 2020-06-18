type kind =
  | Etype (* type error for values *)
  | Estate (* type error for states *)
  | Eunbound_ident of Ident.t (* unbound variable *)
  | Eunbound_lident of Lident.t (* unbound global variable *)
  | Eshould_be_last of Ident.t
  (* [x] should be a state variable with last *)
  | Eshould_be_node of Lident.t (* [x] should be a node *)
  | Eshould_be_value of Lident.t (* [x] should be a value *)
  | Eand_non_linear of Ident.t (* [x] appears twice *)
  | Eno_default of Ident.t (* no default value is given to [x] *)
  | Einitial_state_with_parameter of Ident.t 
  | Eassert_false (* an error that should not appear; [= assert false] *)
  | Euncausal (* some bottom value remain *)
  | Euninitialised (* some nil value remain *)
                       
type t = Location.t * kind

let error loc kind = Error (loc, kind)

  
