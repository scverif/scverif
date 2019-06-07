open Location
open Utils
open Common



type var = {
    v_name : string;
    v_id   : Uid.t;
    v_loc  : Location.t;
    v_ty   : ty;
  }

let mk_var s loc ty = 
  { v_name = s;
    v_id   = Uid.fresh ();
    v_loc  = loc;
    v_ty   = ty }

module Lbl : sig 
  type t
  val compare : t -> t -> int
  val equal   : t -> t -> bool
  val fresh : string -> t
  val clone : t -> t
  val pp : Format.formatter -> t -> unit
end = struct

  type t = {
      l_name : string;
      l_id   : Uid.t;
    }

  let compare l1 l2 = Uid.compare l1.l_id l2.l_id
  let equal l1 l2 = Uid.equal l1.l_id l2.l_id

  let fresh l_name = 
    { l_name;
      l_id = Uid.fresh();
    }

  let clone l = fresh l.l_name

  let pp fmt l = Format.fprintf fmt "%s" l.l_name 

end

module Sl = Set.Make(Lbl)

type leak_info = string option

type op_desc = 
  | Oif  of ty
  | Oadd of wsize option
  | Omul of wsize option
  | Omulh of wsize 
  | Osub of wsize option
  | Oopp of wsize option
  | Olsl of wsize
  | Olsr of wsize
  | Oasr of wsize 
  | Oand of wsize option
  | Oxor of wsize option
  | Oor  of wsize option
  | Onot of wsize option
  | Oeq  of bty 
  | Olt  of sign * wsize option
  | Ole  of sign * wsize option
  | Osignextend of wsize * wsize 
  | Ozeroextend of wsize * wsize 
  | Ocast_int   of sign * wsize 
  | Ocast_w     of wsize 

type op = {
    od   : op_desc;
  }

type expr = 
  | Eint of B.zint 
  | Ebool of bool
  | Evar of var 
  | Eget of var * expr            (* array access *)
  | Eload of wsize * var * expr   (* memory access *)
  | Eop  of op * expr list

type lval = 
  | Lvar   of var        
  | Lset   of var * expr           (* array assign *)
  | Lstore of wsize * var * expr   (* memory assign *)
  
type macro_arg = 
  | Aexpr  of expr 
  | Alabel of Lbl.t
  | Aindex of var * B.zint * B.zint   

type param = 
  | Pvar of var 
  | Plabel of Lbl.t

type instr_desc = 
  | Iassgn of lval * expr 
  | Ileak  of leak_info * expr list
  | Imacro of macro * macro_arg list 
  | Ilabel of Lbl.t 
  | Igoto  of Lbl.t
  | Iif    of expr * cmd * cmd 
  | Iwhile of cmd * expr * cmd 
  
and instr = {
   i_desc : instr_desc;
   i_loc  : full_loc;
  }

and cmd = instr list

and macro = {
    mc_name   : string;
    mc_id     : Uid.t;
    mc_loc    : Location.t;
    mc_params : param list;
    mc_locals : param list;
    mc_body   : cmd;
  }

type global = 
  | Gvar   of var 
  | Gmacro of macro


(* *********************************************** *)

let destr_var e = 
  match e with
  | Evar x -> x
  | _      -> assert false

(* *********************************************** *)

let defined_label c = 
  List.fold_left (fun s i ->
    match i.i_desc with
    | Ilabel lbl -> Sl.add lbl s 
    | _ -> s) Sl.empty c

let params_label = 
  List.fold_left (fun s p ->
      match p with
      | Plabel lbl -> Sl.add lbl s
      | _ -> s) Sl.empty 
  
let check_labels msg m = 
  let def = defined_label m.mc_body in
  let dparams = params_label m.mc_params in
  let all = Sl.union def dparams in
  let check ii lbl = 
    if not (Sl.mem lbl all) then
      error msg ii "undefined label %a" Lbl.pp lbl in
  let check_args ii args = 
    List.iter (function Alabel lbl -> check ii lbl | _ -> ()) args in
  let rec check_i i =
    match i.i_desc with
    | Iassgn _ | Ileak _ | Ilabel _ -> ()
    | Imacro(_,args) -> check_args i.i_loc args 
    | Igoto lbl -> check i.i_loc lbl 
    | Iif(_,c1,c2) | Iwhile(c1,_,c2) -> check_c c1; check_c c2 
  and check_c c = List.iter check_i c in
  check_c m.mc_body
                    
