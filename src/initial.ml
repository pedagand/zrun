open Value
open Monad
open Opt
open Lident
   
module Genv = Map.Make(Lident)

let boolean v =
  match v with
  | Vbool(b) -> return b
  | _ -> None

let integer v =
  match v with
  | Vint(i) -> return i | _ -> None

let ifthenelse_op v v1 v2 =
  let* b = boolean v in
  if b then return v1 else return v2

let not_op v =
  let* v = boolean v in
  return (Vbool(not v))

let uminus_op v =
  let* v = integer v in
  return (Vint(- v))

let and_op v1 v2 =
  let* v1 = boolean v1 in
  let* v2 = boolean v2 in
  return (Vbool(v1 && v2))

let or_op v1 v2 =
  let* v1 = boolean v1 in
  let* v2 = boolean v2 in
  return (Vbool(v1 || v2))

let add_op v1 v2 =
  let* v1 = integer v1 in
  let* v2 = integer v2 in
  return (Vint(v1 + v2))

let minus_op v1 v2 =
  let* v1 = integer v1 in
  let* v2 = integer v2 in
  return (Vint(v1 - v2))
    
let mult_op v1 v2 =
  let* v1 = integer v1 in
  let* v2 = integer v2 in
  return (Vint(v1 * v2))

let div_op v1 v2 =
  let* v1 = integer v1 in
  let* v2 = integer v2 in
  return (Vint(v1 / v2))

let mod_op v1 v2 =
  let* v1 = integer v1 in
  let* v2 = integer v2 in
  return (Vint(v1 mod v2))

let mod_eq v1 v2 =
  return (Vbool(v1 = v2))

let geti i v =
  let rec geti i v_list =
    match v_list with
    | [] -> None
    | v :: v_list -> if i = 0 then Some(v) else geti (i-1) v_list in
  match v with
  | Vbot -> return Vbot
  | Vnil -> return Vnil
  | Value(v) ->
     match v with
     | Vtuple(v_list) -> if i >= 0 then geti i v_list else None
     | _ -> None
          
(* ifthenelse *)
let ifthenelse v1 v2 v3 =
  match v1, v2, v3 with
  | Vbot, _, _ -> return Vbot
  | (Vnil, Vbot, _) | (Vnil, _, Vbot) -> return Vbot
  | Vnil, _, _ -> return Vnil
  | (Value(v1), _, _)  -> ifthenelse_op v1 v2 v3
                 
(* lift a unary operator: [op bot = bot]; [op nil = nil] *)
let lift1 op v =
  match v with
  | Vbot -> return Vbot
  | Vnil -> return Vnil
  | Value(v) ->
     let* v = op v in
     return (Value v)

(* check that the input is non bot/nil *)
let value v =
  match v with
  | Vbot | Vnil -> None
  | Value(v) -> return v
              
(* lift a binary operator: [op bot _ = bot]; [op _ bot = bot]; same for nil *)
let lift2 op v1 v2 =
  match v1, v2 with
  | Vbot, _ -> return Vbot
  | _, Vbot -> return Vbot
  | Vnil, _ -> return Vnil
  | _, Vnil -> return Vnil
  | Value(v1), Value(v2) ->
     let* v = op v1 v2 in
     return (Value v)

(* pairs and tuples *)
let unbot v1 v2 = 
  match v1, v2 with
  | (Vbot, _) | (_, Vbot) -> None
  | _ -> Some (v1, v2)

let rec unbot_list v_list =
  match v_list with
  | [] -> return []
  | [v] -> let* v = match v with | Vbot -> None | _ -> Some([v]) in
           return v
  | v1 :: v2 :: v_list ->
     let* v_list = unbot_list v_list in
     match unbot v1 v2 with
     | None -> None
     | Some(v1, v2) -> return (v1 :: v2 :: v_list)

let unnil v1 v2 = 
  match v1, v2 with
  | (Vnil, _) | (_, Vnil) -> None
  | _ -> Some (v1, v2)
 
let rec unnil_list v_list =
  match v_list with
  | [] -> return []
  | [v] -> let* v = match v with | Vnil -> None | _ -> Some([v]) in
           return v
  | v1 :: v2 :: v_list ->
     let* v_list = unnil_list v_list in
     match unnil v1 v2 with
     | None -> None
     | Some(v1, v2) -> return (v1 :: v2 :: v_list)

(* builds a pair. If one is bot, the result is bot; if one is nil, *)
(* the result is nil *)
let strict_pair v1 v2 =
  let v = unbot v1 v2 in
  match v with
  | None -> Vbot
  | Some(v1, v2) ->
     let v = unnil v1 v2 in
     match v with
     | None -> Vnil
     | Some(v1, v2) -> Value(Vtuple [v1; v2])
                     
let strict_tuple v_list =
  let v = unbot_list v_list in
  match v with
  | None -> Vbot
  | Some(v_list) ->
     let v = unnil_list v_list in
     match v with
     | None -> Vnil
     | Some(v_list) -> Value(Vtuple(v_list))

let strict_constr1 f v_list =
  let v = unbot_list v_list in
  match v with
  | None -> Vbot
  | Some(v_list) ->
     let v = unnil_list v_list in
     match v with
     | None -> Vnil
     | Some(v_list) -> Value(Vconstr1(f, v_list))

let tuple v_list =
  match v_list with
  | [v] -> v
  | _ -> Value(Vtuple(v_list))
       

(* check that v is a list of length one *)
let one v =
  match v with
  | [v] -> return v
  | _ -> None

(* check that v is a list of length two *)
let two v =
  match v with
  | [v1;v2] -> return (v1, v2)
  | _ -> None
       
let unop op =
  CoFun
    (fun v -> let* v = one v in let* v = lift1 op v in return [v])

let binop op =
  CoFun
    (fun v -> let* (v1, v2) = two v in let* v = lift2 op v1 v2 in return [v])
  
  (* The initial environment *)
let genv0 =
  ["+", binop add_op;
   "-", binop minus_op;
   "~-", unop uminus_op;
   "-", binop minus_op;
   "/", binop div_op;
   "*", binop mult_op;
   "not", unop not_op;
   "&&", binop and_op;
   "&", binop and_op;
   "or", binop or_op;
   "||", binop or_op;
   "mod", binop mod_op;
   "=", binop mod_eq]

let genv0 =
  List.fold_left
    (fun acc (n, v) -> Genv.add (Name n) (Gfun v) acc) Genv.empty genv0
   
