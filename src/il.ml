(* Copyright 2019-2020 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)

open Location
open Utils
open Common

type var = {
    v_name : string;
    v_id   : Uid.t;
    v_loc  : Location.t;
    v_ty   : ty;
  }

module V : sig
  type t = var

  val compare : t -> t -> int
  val equal   : t -> t -> bool
  val fresh   : string -> Location.t -> ty -> t
  val clone   : t -> t
  val pp_full : full:bool -> Format.formatter -> t -> unit
  val pp_dbg  : Format.formatter -> t -> unit
  val pp      : Format.formatter -> t -> unit
  val pp_g    : Format.formatter -> t -> unit

end = struct

  type t = var

  let compare v1 v2 = Uid.compare v1.v_id v2.v_id
  let equal v1 v2 = Uid.equal v1.v_id v2.v_id

  let fresh s loc ty = {
      v_name = s;
      v_id   = Uid.fresh ();
      v_loc  = loc;
      v_ty   = ty
    }

  let clone x = { x with v_id = Uid.fresh () }

  let pp_full ~full fmt v =
    if full then
      Format.fprintf fmt "%s.%a" v.v_name Uid.pp v.v_id
    else Format.fprintf fmt "%s" v.v_name

  let pp fmt v = pp_full ~full:false fmt v

  let pp_dbg fmt v = pp_full ~full:true fmt v

  let pp_g fmt v = pp_full ~full:!Glob_option.full fmt v

end

module Mv = Map.Make(V)
module Sv = Set.Make(V)

type lbl = {
  l_name : string;
  l_offs : B.zint;
  l_id   : Uid.t;
}

module Lbl : sig
  type t = lbl
  val compare : t -> t -> int
  val equal   : t -> t -> bool
  val fresh   : string -> B.zint -> t
  val clone   : t -> t
  val pp_full : full:bool -> Format.formatter -> t -> unit
  val pp      : Format.formatter -> t -> unit
  val pp_g    : Format.formatter -> t -> unit
  val pp_dbg  : Format.formatter -> t -> unit

  val exit_ : t
end = struct

  type t = lbl

  let compare l1 l2 = Uid.compare l1.l_id l2.l_id
  let equal l1 l2 = Uid.equal l1.l_id l2.l_id

  let fresh l_name ofs =
    { l_name;
      l_id = Uid.fresh();
      l_offs = ofs;
    }

  let clone l = fresh l.l_name l.l_offs

  let pp_full ~full fmt l =
    let ln =
      if B.equal B.zero l.l_offs then
        l.l_name
      else
        l.l_name ^ "+" ^ (B.to_string l.l_offs) in
    if full then
      Format.fprintf fmt "%s.%a" ln Uid.pp l.l_id
    else Format.fprintf fmt "%s" ln

  let pp fmt l = pp_full ~full:false fmt l
  let pp_g fmt l = pp_full ~full:!Glob_option.full fmt l
  let pp_dbg fmt l = pp_full ~full:true fmt l

  let exit_ = fresh "exit_macro" B.zero

end

module Sl = Set.Make(Lbl)
module Ml = Map.Make(Lbl)

type leak_info = string option

type op_desc =
  | Oif  of ty
  | Oadd of wsize option
  | Omul of wsize option (* low word of multiplication*)
  | Omulh of wsize (* high word of multiplication*)
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
  | Onamecmp
  | Olt  of sign * wsize option
  | Ole  of sign * wsize option
  | Osignextend of wsize * wsize   (* from, to *)
  | Ozeroextend of wsize * wsize   (* from, to *)
  | Ocast_int   of sign * bty
  | Ocast_w     of wsize

type op = {
    od   : op_desc;
  }

type expr =
  | Eint  of B.zint               (** constant integer *)
  | Ebool of bool                 (** constant boolean *)
  | Evar  of V.t                  (** magic dinosaur *)
  | Eget  of V.t * expr           (** array access *)
  | Eload of wsize * V.t * expr   (** memory access *)
  | Eop   of op * expr list       (** operation on variable number of arguments *)

type lval =
  | Lvar   of V.t
  | Lset   of V.t * expr           (** array assign *)
  | Lstore of wsize * V.t * expr   (** memory assign *)

type macro_arg =
  | Aexpr  of expr
  | Alabel of Lbl.t
  | Aindex of V.t * B.zint * B.zint

type param =
  | Pvar of V.t
  | Plabel of Lbl.t

type instr_desc =
  | Iassgn of lval * expr                  (** assignment to state from expression *)
  | Ileak  of leak_info * expr list        (** explicit leak statement *)
  | Imacro of macro_name * macro_arg list  (** call of macro by name and arguments *)
  | Ilabel of Lbl.t                        (** label, used as target for jumps and data *)
  | Igoto  of Lbl.t                        (** static goto with fixed destination *)
  | Iigoto of V.t                          (** implicit goto of variable destination *)
  | Iif    of expr * cmd * cmd             (** conditional code exection,
                                                1st cmd for true, 2nd for false branch *)
  | Iwhile of cmd * expr * cmd             (** conditional loop with *)

and instr = {
   i_desc : instr_desc;
   i_loc  : full_loc;
  }

and cmd = instr list

and macro_name = string

and macro = {
  mc_name   : macro_name;
  mc_uid    : Uid.t;
  mc_loc    : Location.t;
  mc_params : param list;
  mc_locals : param list;
  mc_body   : cmd;
}

type global =
  | Gvar   of V.t
  | Gmacro of macro

module M = struct
  type t = macro

  let pp_full ~full fmt m =
    if full then
      Format.fprintf fmt "%s.%a" m.mc_name Uid.pp m.mc_uid
    else Format.fprintf fmt "%s" m.mc_name

  let pp fmt m = pp_full ~full:false fmt m
  let compare (m1:t) (m2:t) : int = Uid.compare m1.mc_uid m2.mc_uid

end

module Mb = Map.Make(Z)

(** data associated to a label *)
type lbldata = {
  ld_defmacro : macro_name option;  (** defining macro *)
  ld_data     : B.zint option;      (** optional data associated *)
}

type genv = {
  glob_var : V.t Ms.t;           (** variables which have been defined global *)
  macro    : macro Ms.t;         (** macros which have been defined global *)
  (** two staged lookup of labels by name and offset,
      for computation on labels *)
  glob_lbl : (Lbl.t Mb.t) Ms.t;
  glob_mem : lbldata Ml.t;       (** global memory addressable by labels *)
}

let empty_genv = {
    glob_var = Ms.empty;
    glob_lbl = Ms.empty;
    glob_mem = Ml.empty;
    macro    = Ms.empty;
  }

(* ************************************************* *)
(* Global environment                                *)

let pp_glob_var ~full fmt gvs =
  Format.fprintf fmt "  @[<v>";
  Ms.iter (fun n v ->
      Format.fprintf fmt "%s -> %a@ "
        n (V.pp_full ~full:full) v) gvs;
  Format.fprintf fmt "@]"

let pp_mb_lbl ~full fmt ml =
  Format.fprintf fmt "  {@[<v>";
  Mb.iter (fun b l ->
      Format.fprintf fmt "%a@ "
      (Lbl.pp_full ~full) l) ml;
  Format.fprintf fmt "@]}"

let pp_glob_lbl ~full fmt gls =
  Format.fprintf fmt "  @[<v>";
  Ms.iter (fun ln ml ->
      Format.fprintf fmt "%s -> %a@ "
        ln (pp_mb_lbl ~full) ml) gls;
  Format.fprintf fmt "@]"

let pp_lbldata fmt (d:lbldata) =
  let pp_data fmt = function
   | Some i ->
     B.pp_print fmt i;
   | _ ->
     Format.fprintf fmt "_" in
  let pp_mac fmt = function
    | Some mn ->
      Format.fprintf fmt "%s" mn;
    | _ ->
      Format.fprintf fmt "_" in
  Format.fprintf fmt "%a data, defined in %a"
    pp_data d.ld_data pp_mac d.ld_defmacro

let pp_glob_mem ~full fmt ml =
  Format.fprintf fmt "  {@[<v>";
  Ml.iter (fun l d ->
      Format.fprintf fmt "%a -> %a@ "
      (Lbl.pp_full ~full) l pp_lbldata d) ml;
  Format.fprintf fmt "@]}"

let pp_glob_macro_name ~full fmt mm =
  Format.fprintf fmt "  @[<v>";
  let pp_mc_name fmt m =
    if full then
      Format.fprintf fmt "%s.%a" m.mc_name Uid.pp m.mc_uid
    else
      Format.fprintf fmt "%s" m.mc_name in
  Ms.iter (fun mn m ->
      Format.fprintf fmt "%s -> %a@ "
        mn pp_mc_name m) mm;
  Format.fprintf fmt "@]"

let pp_genv ~full fmt g =
  Format.fprintf fmt "@[<v>global vars:{@   @[<v>%a@]}@ \
                      global labels:{@   @[<v>%a@]}@ \
                      global memory:{@   @[<v>%a@]}@ \
                      global macros:{@   @[<v>%a@]}@ @]"
    (pp_glob_var ~full:full) g.glob_var
    (pp_glob_lbl ~full:full) g.glob_lbl
    (pp_glob_mem ~full:full) g.glob_mem
    (pp_glob_macro_name ~full:full) g.macro

let pp_genv_g fmt g = pp_genv ~full:!Glob_option.full fmt g
let pp_genv_dbg fmt g = pp_genv ~full:true fmt g

(* *********************************************** *)
(* Destructors                                     *)
let destr_var e =
  match e with
  | Evar x -> x
  | _      -> assert false

(* *********************************************** *)
(* Constructors                                    *)

let oaddi = { od = Oadd None }
let osubi = { od = Osub None }

let addi e1 e2 = Eop(oaddi, [e1; e2])

let addi_imm e1 i = addi e1 (Eint i)

let subi e1 e2 = Eop(osubi, [e1; e2])

let subi_imm e1 i = subi e1 (Eint i)

(* *********************************************** *)

let lv2e = function
  | Lvar x -> Evar x
  | Lset(x,e) -> Eget(x,e)
  | Lstore(ws,x,e) -> Eload(ws,x,e)

let e2lv = function
  | Evar x -> Lvar x
  | Eget(x,e) -> Lset(x,e)
  | Eload(ws,x,e) -> Lstore(ws,x,e)
  | _  -> raise Not_found

(* *********************************************** *)
(* Checking that all labels are defined            *)

(** get labels defined in instruction list *)
let defined_label c =
  List.fold_left (fun s i ->
    match i.i_desc with
    | Ilabel lbl -> Sl.add lbl s
    | _ -> s) Sl.empty c

(** get labels declared in macro header *)
let params_label =
  List.fold_left (fun s p ->
      match p with
      | Plabel lbl -> Sl.add lbl s
      | _ -> s) Sl.empty

let find_label_glob (g:genv) (name:string) (offs:B.zint)
  : Lbl.t option =
  try
    let ofs = Ms.find name g.glob_lbl in
    Some (Mb.find (B.to_zint offs) ofs)
  with Not_found ->
    None

(** validity check of all used labels in a macro *)
let check_labels (msg:string) (g:genv) (m:macro) =
  let def = defined_label m.mc_body in
  let dparams = params_label m.mc_params in
  let all = Sl.union def dparams in
  let defined = ref Sl.empty in
  (* check valid use of a label inside macro body *)
  let check (ii:full_loc) (lbl:Lbl.t) =
    (* check if declared as parameter or local *)
    if not (Sl.mem lbl all) then
      (* try to find the label globally *)
      match find_label_glob g lbl.l_name lbl.l_offs with
      | Some l when Lbl.equal lbl l ->
        (* label defined, check its definition *)
        begin
          try
            let lbldata = Ml.find l g.glob_mem in
            (match lbldata.ld_defmacro with
            | Some mn ->
              let definingmacro = Ms.find mn g.macro in
              if not (Sl.mem lbl (params_label definingmacro.mc_locals)) then
                error msg ii "%a@ inconsistent label %a in %s, globally known but undefined in %s"
                  pp_genv_dbg g (Lbl.pp_full ~full:true) lbl m.mc_name definingmacro.mc_name
            | None ->
              error msg ii "%a@ label %a in %s is globally known but \
                            not defined inside a macro"
                  pp_genv_dbg g (Lbl.pp_full ~full:true) lbl m.mc_name)
          with Not_found ->
            error msg ii "%a@ corrupted label database: \
                          %a in %s is globally known but cannot be resolved"
              pp_genv_dbg g (Lbl.pp_full ~full:true) lbl m.mc_name
        end
      | _ ->
        error msg ii
          "%a@ undefined label %a in %s"
          pp_genv_dbg g (Lbl.pp_full ~full:true) lbl m.mc_name
  in
  (* check validity of a label definition *)
  let check_def ii lbl =
    if Sl.mem lbl !defined then
      error msg ii "redefinition of label %a" Lbl.pp lbl;
    defined := Sl.add lbl !defined
  in
  (* check validity of labels used as argument *)
  let check_args ii args =
    List.iter (function Alabel lbl -> check ii lbl | _ -> ()) args in
  let rec check_i i =
    match i.i_desc with
    | Iassgn _ | Ileak _ -> ()
    | Ilabel lbl -> check_def i.i_loc lbl
    | Imacro(_,args) -> check_args i.i_loc args
    | Igoto lbl -> check i.i_loc lbl
    | Iigoto _  -> ()
    | Iif(_,c1,c2) | Iwhile(c1,_,c2) -> check_c c1; check_c c2
  and check_c c = List.iter check_i c in
  check_c m.mc_body



(* ************************************************ *)
(* Pretty printing                                  *)

let op_string op =
  match op.od with
  | Oif   _ -> "if"
  | Oadd  _ -> "+"
  | Omul  _ -> "*"
  | Omulh _ -> "**"
  | Osub  _ -> "-"
  | Oopp  _ -> "-"
  | Olsl  _ -> "<<"
  | Olsr  _ -> ">>"
  | Oasr  _ -> ">>s"
  | Oand  _ -> "&"
  | Oxor  _ -> "^"
  | Oor   _ -> "|"
  | Onot  _ -> "!"
  | Oeq   _ -> "=="
  | Onamecmp -> "=name="
  | Olt   _ -> "<"
  | Ole   _ -> "<="
  | Osignextend _ -> "signextend"
  | Ozeroextend _ -> "zeroextend"
  | Ocast_int   _ -> "cast_int"
  | Ocast_w     _ -> "cast_w"

let rec pp_e ~full fmt e =
  match e with
  | Eint i  -> Format.fprintf fmt "%a" B.pp_print i
  | Ebool b -> Format.fprintf fmt "%b" b
  | Evar v  -> V.pp_full ~full fmt v
  | Eget(x,e) ->
    Format.fprintf fmt "%a[%a]" (V.pp_full ~full) x (pp_e ~full) e
  | Eload(ws, x, e) ->
    Format.fprintf fmt "@[[%a@ %a@ %a]@]"
      pp_wsize ws (V.pp_full ~full) x (pp_e ~full) e
  | Eop(op, es) ->
    match op.od, es with
    | Oif _, [e;e1;e2] ->
      Format.fprintf fmt "@[%a ?@ %a :@ %a@]"
        (pp_be ~full) e (pp_e ~full) e1 (pp_be ~full) e2
    | Oif _, _     -> assert false
    | Oadd ws, es  -> pp_op2_ows ~full "+" fmt ws es
    | Omul ws, es  -> pp_op2_ows ~full "*" fmt ws es
    | Omulh ws, es -> pp_op2_ws ~full "**" fmt ws es
    | Osub ws, es  -> pp_op2_ows ~full "-" fmt ws es
    | Oopp ws, es  -> pp_op1_ows ~full "-" fmt ws es
    | Olsl ws, es  -> pp_op2_ws  ~full "<<" fmt ws es
    | Olsr ws, es  -> pp_op2_ws  ~full ">>" fmt ws es
    | Oasr ws, es  -> pp_op2_ws  ~full ">>s" fmt ws es
    | Oand ws, es  -> pp_op2_ows ~full "&" fmt ws es
    | Oxor ws, es  -> pp_op2_ows ~full "^" fmt ws es
    | Oor  ws, es  -> pp_op2_ows ~full "|" fmt ws es
    | Onot ws, es  -> pp_op1_ows ~full "!" fmt ws es
    | Oeq  ws, es  -> pp_op2     ~full "==" fmt es
    | Onamecmp, es -> pp_op2     ~full "=name=" fmt es
    | Olt(s,ws),es ->
      let s = if s = Signed then "<s" else "<" in
      pp_op2_ows ~full s fmt ws es
    | Ole(s,ws),es ->
      let s = if s = Signed then "<=s" else "<=" in
      pp_op2_ows ~full s fmt ws es
    | Osignextend(_, ws), es ->
      pp_op1_ws ~full "signextend" fmt ws es
    | Ozeroextend(_, ws), es ->
      pp_op1_ws ~full "zeroextend" fmt ws es
    | Ocast_int(s,_), es ->
      let s = if s = Signed then "(int)" else "(uint)" in
      pp_op1 ~full s fmt es
    | Ocast_w ws, es ->
      let s = Format.sprintf "(%s)" (ws_string ws) in
      pp_op1 ~full s fmt es

and pp_be ~full fmt e =
  match e with
  | Eint _ | Ebool _ | Evar _ | Eget _ | Eload _ -> pp_e ~full fmt e
  | Eop _ -> Format.fprintf fmt "(%a)" (pp_e ~full) e

and pp_op2 ~full s fmt es =
  match es with
  | [e1; e2] ->
    Format.fprintf fmt "@[%a %s@ %a@]" (pp_be ~full) e1 s (pp_be ~full) e2
  | _ -> assert false

and pp_op2_ws ~full s fmt ws es =
  let s = Format.sprintf "%s%s" s (ws_string ws) in
  pp_op2 ~full s fmt es

and pp_op2_ows ~full s fmt ws es =
  match ws with
  | None -> pp_op2 ~full s fmt es
  | Some ws -> pp_op2_ws ~full s fmt ws es

and pp_op1 ~full s fmt es =
  match es with
  | [e1] ->
    Format.fprintf fmt "@[%s@ %a@]" s (pp_be ~full) e1
  | _ -> assert false

and pp_op1_ws ~full s fmt ws es =
  let s = Format.sprintf "%s%s" s (ws_string ws) in
  pp_op1 ~full s fmt es

and pp_op1_ows ~full s fmt ws es =
  match ws with
  | None -> pp_op1 ~full s fmt es
  | Some ws -> pp_op1_ws ~full s fmt ws es

let pp_e_g fmt e = pp_e ~full:!Glob_option.full fmt e
let pp_e_dbg fmt e = pp_e ~full:true fmt e

let pp_lval ~full fmt lv =
  match lv with
  | Lvar x ->
    Format.fprintf fmt "%a" (V.pp_full ~full) x
  | Lset(x,e) ->
    Format.fprintf fmt "%a[%a]" (V.pp_full ~full) x (pp_e ~full) e
  | Lstore(ws, x, e) ->
    Format.fprintf fmt "@[[%a@ %a@ %a]@]"
      pp_wsize ws (V.pp_full ~full) x (pp_e ~full) e

let pp_marg ~full fmt = function
  | Aexpr e -> pp_e ~full fmt e
  | Alabel lbl -> Lbl.pp_full ~full fmt lbl
  | Aindex (x,i1,i2) ->
    Format.fprintf fmt "%a[%a:%a]"
      (V.pp_full ~full) x B.pp_print i1 B.pp_print i2

let pp_margs ~full fmt args =
  Format.fprintf fmt "@[(%a)@]"
    (pp_list ",@ " (pp_marg ~full)) args

let pp_var_decl ~full fmt x =
  match x.v_ty with
  | Tmem -> Format.fprintf fmt "%a[]" (V.pp_full ~full) x
  | Tarr(bty,i1,i2) ->
    Format.fprintf fmt "%a %a[%a:%a]"
      pp_bty bty (V.pp_full ~full) x B.pp_print i1 B.pp_print i2
  | Tbase bty ->
    Format.fprintf fmt "%a %a"
      pp_bty bty (V.pp_full ~full) x

let pp_param ~full fmt = function
  | Pvar x -> pp_var_decl ~full fmt x
  | Plabel lbl -> Format.fprintf fmt "label %a" (Lbl.pp_full ~full) lbl

let pp_params ~full fmt ps =
  Format.fprintf fmt "@[%a@]" (pp_list ",@ " (pp_param ~full)) ps

let pp_leak_info fmt i =
  match i with
  | None -> ()
  | Some s -> Format.fprintf fmt "%s" s

let rec pp_i ~full fmt i =
  match i.i_desc with
  | Iassgn(x,e) ->
    Format.fprintf fmt "@[%a <-@ %a;@]"
      (pp_lval ~full) x (pp_e ~full) e
  | Ileak(i, es) ->
    Format.fprintf fmt "@[leak %a (%a);@]"
    pp_leak_info i (pp_list ",@ " (pp_e ~full)) es
  | Imacro(mcname, args) ->
    if full then
      Format.fprintf fmt "@[%s%a;@]"
        mcname (pp_margs ~full) args
    else
      Format.fprintf fmt "@[%s%a;@]"
        mcname (pp_margs ~full) args

  | Ilabel lbl ->
    Format.fprintf fmt "%a:" (Lbl.pp_full ~full) lbl
  | Igoto lbl ->
    Format.fprintf fmt "goto %a;" (Lbl.pp_full ~full) lbl
  | Iigoto x ->
    Format.fprintf fmt "goto %a;" (V.pp_full ~full) x
  | Iif(e,c1,c2) ->
    Format.fprintf fmt "@[<v>if %a@ %a%a@]"
      (pp_e ~full) e
      (pp_cmd ~full) c1
      (pp_else ~full) c2
  | Iwhile(c1, e, c2) ->
    let pp_c1 fmt c1 =
      if c1 == [] then Format.fprintf fmt " "
      else Format.fprintf fmt "@ %a@ " (pp_cmd ~full) c1 in
    let pp_c2 fmt c2 =
      if c2 == [] then ()
      else Format.fprintf fmt "@ %a" (pp_cmd ~full) c2 in
    Format.fprintf fmt "@[<v>while%a(%a)%a@]"
      pp_c1 c1 (pp_e ~full) e pp_c2 c2

and pp_cmd ~full fmt c =
  Format.fprintf fmt "@[<v>{@   @[<v>%a@]@ }@]"
    (pp_list "@ " (pp_i ~full)) c

and pp_else ~full fmt c =
  if c == [] then ()
  else
    Format.fprintf fmt "@ else@ %a" (pp_cmd ~full) c

let pp_cmd_g fmt = pp_cmd ~full:!Glob_option.full fmt
let pp_cmd_dbg fmt = pp_cmd ~full:true fmt
let pp_i_g fmt = pp_i ~full:!Glob_option.full fmt
let pp_i_dbg fmt = pp_i ~full:true fmt

let pp_macro ~full fmt m =
  let pp_locals fmt locals =
    if locals == [] then ()
    else Format.fprintf fmt "  %a@ " (pp_params ~full) locals in
  Format.fprintf fmt "@[<v>macro %a(%a)@ %a%a@]"
    (M.pp_full ~full) m (pp_params ~full) m.mc_params
    pp_locals m.mc_locals
    (pp_cmd ~full) m.mc_body

let pp_macro_g p =
  pp_macro ~full:!Glob_option.full p

let pp_macro_dbg p =
  pp_macro ~full:true p

let pp_global ~full fmt = function
  | Gvar x -> Format.fprintf fmt "%a;" (pp_var_decl ~full) x
  | Gmacro m -> pp_macro ~full fmt m

let pp_global_g fmt = pp_global ~full:!Glob_option.full fmt

let pp_globals ~full fmt gs =
  Format.fprintf fmt "@[<v>%a@]"
   (pp_list "@ @ " (pp_global ~full)) gs

(* ************************************************************* *)
let check_size i1 i2 j1 j2 =
  B.equal (B.sub i2 i1) (B.sub j2 j1)


(* ************************************************************* *)
(* Remove unused local variables                                 *)

let add_used_lbl (ulbl, uvar) lbl = (Sl.add lbl ulbl, uvar)
let add_used_var (ulbl, uvar) x   = (ulbl, Sv.add x uvar)

let rec used_e used = function
  | Eint _ | Ebool _ -> used
  | Evar x -> add_used_var used x
  | Eget(x,e) | Eload(_,x,e) -> used_e (add_used_var used x) e
  | Eop(_,es) -> used_es used es

and used_es used es =List.fold_left used_e used es

let used_arg used = function
  | Aexpr e -> used_e used e
  | Alabel lbl -> add_used_lbl used lbl
  | Aindex(x,_,_) -> add_used_var used x

let used_lval used = function
  | Lvar x -> add_used_var used x
  | Lset (x,e) | Lstore(_,x,e) -> used_e (add_used_var used x) e

let rec used_i used i = 
  match i.i_desc with
  | Iassgn(x,e) -> used_e (used_lval used x) e
  | Ileak(_, es) -> used_es used es
  | Imacro(_, margs) -> List.fold_left used_arg used margs
  | Ilabel lbl | Igoto lbl -> add_used_lbl used lbl
  | Iigoto x -> add_used_var used x
  | Iif(e,c1,c2) | Iwhile(c1,e,c2) ->
    used_c (used_c (used_e used e) c1) c2

and used_c used c = List.fold_left used_i used c

let clear_macro m = 
  let (ulbl, uvar) = used_c (Sl.empty, Sv.empty) m.mc_body in
  let is_used = function
    | Pvar x -> Sv.mem x uvar 
    | Plabel lbl -> Sl.mem lbl ulbl in
  { m with mc_locals = List.filter is_used m.mc_locals }
