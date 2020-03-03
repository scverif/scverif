(* Copyright 2019-2020 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear *)

open Location
open Utils
open Common
open Il

let ev_hierror (loc:full_loc) fmsg = error "evaluation" loc fmsg

type pointer = [%import: Ileval.pointer]
type cpointer = [%import: Ileval.cpointer]
type bvalue = [%import: Ileval.bvalue]
type value = [%import: Ileval.value]
type state = [%import: Ileval.state]
type region = [%import: Ileval.region]
type ival = [%import: Ileval.ival]
type t_ty = [%import: Ileval.t_ty]
type rdecl =[%import: Ileval.rdecl]
type rdecls =[%import: Ileval.rdecls]
type initial = [%import: Ileval.initial]
type eenv = [%import: Ileval.eenv]

module Ptr = struct
  type t = pointer
  let equal p1 p2 =
    V.equal p1.p_mem  p2.p_mem  &&
    V.equal p1.p_dest p2.p_dest &&
    B.equal p1.p_ofs  p2.p_ofs

  let pp fmt p =
    Format.fprintf fmt "[%a @@ %a %a]"
      V.pp_g p.p_mem
      V.pp_g p.p_dest
      B.pp_print p.p_ofs
end

(* Pretty printer *)

let pp_bvalue fmt = function
  | Vcptr lbl -> Format.fprintf fmt "lbl %a" Lbl.pp_g lbl
  | Vptr p    -> Ptr.pp fmt p
  | Vint i    -> B.pp_print fmt i
  | Vbool b   -> Format.fprintf fmt "%b" b
  | Vunknown  -> Format.fprintf fmt "_"

let pp_value fmt = function
  | Varr t ->
    Format.fprintf fmt "@[{%a}@]"
      (pp_list ",@ " pp_bvalue) (Array.to_list t)
  | Vbase bv -> pp_bvalue fmt bv

let pp_regions fmt mr =
  Format.fprintf fmt "  @[<v>";
  Mv.iter (fun x t ->
      Format.fprintf fmt "%a -> @[%a@]@ "
           V.pp_g x
           (pp_list ",@ " pp_bvalue) (Array.to_list t)) mr;
  Format.fprintf fmt "@]"

let pp_vars fmt mv =
  Format.fprintf fmt "  @[<v>";
  Mv.iter (fun x v ->
      Format.fprintf fmt "%a -> %a@ "
        V.pp_g x pp_value v) mv;
  Format.fprintf fmt "@]"

let pp_state fmt st =
  Format.fprintf fmt "@[<v>regions:@ %a@ vars:@ %a@ pc:@ %a@ eprog:@ %a@]"
    pp_regions st.st_mregion
    pp_vars    st.st_mvar
    (pp_cmd ~full:!Glob_option.full) st.st_pc
    (pp_cmd ~full:!Glob_option.full) (List.rev st.st_eprog)

let vars_of_scvtarget (st:state) (t:Scv.scvtarget) : (var * value) list =
  match t with
  | Scv.TIdent [] -> []
  | Scv.TIdent ns ->
    let mvars = Mv.bindings st.st_mvar in
    let mregs = Mv.bindings st.st_mregion in
    let (f, unfound : (var * value) list * string located list) =
      List.fold_left
        (fun (f,nf) (n:string located) ->
           let mv = List.find_opt (fun (k,bva) -> String.equal k.v_name (unloc n)) mvars in
           let mr = List.find_opt (fun (k,bva) -> String.equal k.v_name (unloc n)) mregs in
           match mv, mr with
           | Some mv, Some (k,bv) -> mv::(k,Varr bv)::f,nf
           | None   , Some (k,bv) ->     (k,Varr bv)::f,nf
           | Some m , None        -> m              ::f,nf
           | None   , None        -> f,n            ::  nf)
        ([],[]) ns in
    if List.length unfound == 0 then
      f
    else
      Utils.hierror "Ileval.vars_of_scvtarget" (Some (loc (List.hd unfound)))
        "state contains no definition of @[<v>%a@]"
        (pp_list ",@," Scv.pp_scvstring) unfound
  | Scv.TWildcard _ ->
    Mv.bindings st.st_mvar @
    (List.map (fun (k,bva) -> k,Varr bva) (Mv.bindings st.st_mregion))
  | Scv.TRegex r ->
    let regex = Re.execp (Re.compile (Re.Glob.glob (unloc r))) in
    Mv.fold (fun k bva ms ->
        if regex k.v_name then (k,bva)::ms else ms) st.st_mvar []
    |> Mv.fold (fun k bva ms ->
        if regex k.v_name then (k, Varr bva)::ms else ms) st.st_mregion

let pp_statevars fmt (st,vtgt : state * Scv.scvtarget) : unit =
  let vars = List.rev (vars_of_scvtarget st vtgt) in
  let rec pp_varval fmt (va : (var * value) list) : unit =
    match va with
    | [] -> ()
    | [v,bv] ->
      Format.fprintf fmt "@[%a => %a@]"
        V.pp_g v pp_value bv
    | (v,bv)::va ->
      Format.fprintf fmt "@[%a => %a@]@ %a"
        V.pp_g v pp_value bv pp_varval va
  in
  pp_varval fmt vars

let pp_iregions fmt ir =
  Format.fprintf fmt "  @[<v>";
  List.iter (fun r ->
      Format.fprintf fmt "%a -> @[%a@]@ "
        V.pp_g r.r_from
        V.pp_g r.r_dest) ir;
  Format.fprintf fmt "@]"

let rec pp_ival fmt = function
  | Iint i       -> B.pp_print fmt i
  | Ibool b      -> Format.fprintf fmt "%b" b
  | Ilbl l       -> Format.fprintf fmt "%a" Lbl.pp_g l
  | Iarr iv      -> Format.fprintf fmt "@[[%a]@]"
                      (pp_list ",@," pp_ival) iv
  | Iptr(x,i) ->
    Format.fprintf fmt "[%a %a]"
      V.pp_g x
      B.pp_print i
  | Icptr_exit   -> Format.fprintf fmt "exitlabel "

let pp_ivars fmt iv =
  Format.fprintf fmt "  @[<v>";
  List.iter (fun (x,i) ->
      Format.fprintf fmt "%a = @[%a@]@ "
        V.pp_g x
        pp_ival i) iv;
  Format.fprintf fmt "@]"

let pp_t_ty fmt = function
  | Sharing -> Format.fprintf fmt "sharing"
  | URandom -> Format.fprintf fmt "urandom"
  | Public  -> Format.fprintf fmt "public"
  | Secret  -> Format.fprintf fmt "secret"

let pp_iovars fmt iov =
  let pp_rd fmt = function
    | RDvar x -> V.pp_g fmt x
    | RDget (x,i) -> Format.fprintf fmt "%a[%a]" V.pp_g x B.pp_print i in
  let pp fmt (t,x,rds) = 
    Format.fprintf fmt "%a %a [@[%a@]]"
      pp_t_ty t
      V.pp_g x
      (pp_list ";@ " pp_rd) (Array.to_list rds) in
  Format.fprintf fmt "  @[<v>%a@]" (pp_list "@ " pp) iov

let pp_initial fmt ii =
  Format.fprintf fmt "@[<v>regions:@ %a@ vars:@ %a@ inputs:@ %a@ outputs:@ %a@]"
    pp_iregions ii.init_region
    pp_ivars ii.init_var
    pp_iovars ii.input_var
    pp_iovars ii.output_var


(* *********************************************************** *)
(* Word operations                                             *)

let b8  = B.lshl B.one 8
let b16 = B.lshl B.one 16
let b32 = B.lshl B.one 32
let b64 = B.lshl B.one 64

let basis = function
  | U8  -> b8
  | U16 -> b16
  | U32 -> b32
  | U64 -> b64

let maxsigned_word b = B.sub (B.ashr b 1) B.one

let maxsigned8  = maxsigned_word b8
let maxsigned16 = maxsigned_word b16
let maxsigned32 = maxsigned_word b32
let maxsigned64 = maxsigned_word b64

let maxsigned_w = function
  | U8  -> maxsigned8
  | U16 -> maxsigned16
  | U32 -> maxsigned32
  | U64 -> maxsigned64

let of_int  ws i = B.erem i (basis ws)
let to_uint ws i = B.erem i (basis ws)
let to_int ws i =
  let i = to_uint ws i in
  if B.le i (maxsigned_w ws) then i
  else B.sub i (basis ws)

let to_int_s s ws i =
  if s = Signed then to_int ws i
  else to_uint ws i

(* ***************************************************** *)
(* Operators evaluation                                  *)

(** returns false if exprs are not variables *)
let equal_vars (e1:expr) (e2:expr) : bool =
  match e1, e2 with
  | Evar v1, Evar v2 -> V.equal v1 v2
  | _                -> false

let op_w_w op ws i1 =
  let w1 = of_int ws i1 in
  Vint (to_uint ws (op w1))

let op_ww_w op ws i1 i2 =
  let w1 = of_int ws i1 in
  let w2 = of_int ws i2 in
  Vint (to_uint ws (op w1 w2))

let op_wi_w op ws i1 i2 =
  let w1 = of_int ws i1 in
  let (w2:int) = B.to_int (to_uint ws i2) in
  Vint (to_uint ws (op w1 w2))

let op_ww_b op ws i1 i2 =
  let w1 = of_int ws i1 in
  let w2 = of_int ws i2 in
  Vbool (op w1 w2)

let eif (v1,v2,v3) =
  match v1 with
  | Vbool b -> if b then v2 else v3
  | _       -> Vunknown

let eadd (loc:full_loc) ws (v1, v2) =
  match ws, v1, v2 with
  | None   , Vint i1, Vint i2 -> Vint (B.add i1 i2)
  | _      , Vptr p , Vint i
  | _      , Vint i , Vptr p  -> Vptr { p with p_ofs = B.add p.p_ofs i }
  | Some ws, Vint i1, Vint i2 -> op_ww_w B.add ws i1 i2
  | _                         ->
    Format.printf "@[<v>eadd: cannot evaluate Oadd %a %a in %a@]@."
      pp_bvalue v1 pp_bvalue v2
      pp_full_loc loc;
    Vunknown

let esub ws (v1, v2) =
  match ws, v1, v2 with
  | None   , Vint i1, Vint i2 -> Vint (B.sub i1 i2)
  | _      , Vptr p , Vint i
  | _      , Vint i , Vptr p  -> Vptr { p with p_ofs = B.sub p.p_ofs i }
  | _      , Vptr p1, Vptr p2 ->
    if p1.p_mem == p2.p_mem && p1.p_dest == p2.p_dest then
      Vint (B.sub p1.p_ofs p2.p_ofs)
    else
      begin
        Format.printf "@[<v>esub: cannot evaluate Osub of two different ptr %a %a@]@."
          pp_bvalue v1 pp_bvalue v2;
        Vunknown
      end
  | Some ws, Vint i1, Vint i2 -> op_ww_w B.sub ws i1 i2
  | _                         ->
    Glob_option.print_full "@[<v>esub: cannot evaluate Osub %a %a@]@."
      pp_bvalue v1 pp_bvalue v2;
    Vunknown

let eopp ws v1 =
  match ws, v1 with
  | None   , Vint i -> Vint (B.neg i)
  | Some ws, Vint i -> op_w_w B.neg ws i
  | _                  ->
    Glob_option.print_full "@[<v>eopp: cannot evaluate Oopp %a@]@."
      pp_bvalue v1;
    Vunknown

let emul ws (v1,v2) =
  match ws, v1, v2 with
  | None, Vint i1, Vint i2    -> Vint (B.mul i1 i2)
  | Some ws, Vint i1, Vint i2 -> op_ww_w B.mul ws i1 i2
  | _                  ->
    Glob_option.print_full "@[<v>emul: cannot evaluate Omul %a %a@]@."
      pp_bvalue v1 pp_bvalue v2;
    Vunknown
let elsl ws (v1, v2) =
  match v1, v2 with
  | Vint i1 , Vint i2  -> op_wi_w B.lshl ws i1 i2
  | _                  ->
    Glob_option.print_full "@[<v>elsl: cannot evaluate Olsl %a %a@]@."
      pp_bvalue v1 pp_bvalue v2;
    Vunknown

let elsr ws (v1, v2) =
  match v1, v2 with
  | Vint i1 , Vint i2  -> op_wi_w B.lshr ws i1 i2
  | _                  ->
    Glob_option.print_full "@[<v>elsr: cannot evaluate Olsr %a %a@]@."
      pp_bvalue v1 pp_bvalue v2;
    Vunknown

let easr ws (v1, v2) =
  match v1, v2 with
  | Vint i1 , Vint i2  -> op_wi_w B.ashr ws i1 i2 (* Check this *)
  | _                           ->
    Glob_option.print_full "@[<v>easr: cannot evaluate Oasr %a %a@]@."
      pp_bvalue v1 pp_bvalue v2;
    Vunknown

let eand ws (v1, v2) (e1, e2) =
  match ws, v1, v2 with
  | None   , Vbool b1, Vbool b2 -> Vbool (b1 && b2)
  | Some ws, Vint i1 , Vint i2  -> op_ww_w B.lgand ws i1 i2
  | _      , _       , _        when equal_vars e1 e2 ->
    v1
  | _                           ->
    Glob_option.print_full "@[<v>eand: cannot evaluate Oand %a %a@]@."
      pp_bvalue v1 pp_bvalue v2;
    Vunknown

let exor ws (v1, v2) (e1, e2) =
  match ws, v1, v2 with
  | None   , Vbool b1, Vbool b2 -> Vbool (if b1 then not b2 else b2)
  | Some ws, Vint i1 , Vint i2  -> op_ww_w B.lgxor ws i1 i2
  | None   , _       , _        when equal_vars e1 e2 ->
    Vbool false
  | Some _ , _       , _        when equal_vars e1 e2 ->
    Vint B.zero
  | _                           ->
    Glob_option.print_full "@[<v>exor: cannot evaluate Oxor %a %a@]@."
      pp_bvalue v1 pp_bvalue v2;
    Vunknown

let eor ws (v1, v2) (e1, e2) =
  match ws, v1, v2 with
  | None   , Vbool b1, Vbool b2 -> Vbool (b1 || b2)
  | Some ws, Vint i1 , Vint i2  -> op_ww_w B.lgor ws i1 i2
  | _      , _       , _        when equal_vars e1 e2 ->
    v1
  | _                           ->
    Glob_option.print_full "@[<v>eor: cannot evaluate Oor %a %a@]@."
      pp_bvalue v1 pp_bvalue v2;
    Vunknown

let enot ws v =
  match ws, v with
  | None   , Vbool b -> Vbool (not b)
  | Some ws, Vint i  -> op_w_w B.lgnot ws i
  | _                ->
    Glob_option.print_full "@[<v>enot: cannot evaluate Onot %a@]@."
      pp_bvalue v;
    Vunknown


let eeq bty (v1,v2) (e1,e2) =
  match bty, v1, v2 with
  | Int , Vint  i1, Vint  i2 -> Vbool (B.equal i1 i2)
  | _   , Vptr  p1, Vptr  p2 -> Vbool (Ptr.equal p1 p2)
  | Bool, Vbool b1, Vbool b2 -> Vbool (b1 = b2)
  | W ws, Vint  i1, Vint  i2 -> Vbool (B.equal (of_int ws i1) (of_int ws i2))
  | _   , _       , _        when equal_vars e1 e2 -> Vbool true
  | _,    Vptr  _ , _        ->
    Glob_option.print_full "@[<v>eeq: cannot evaluate Oeq of ptr %a %a@]@."
      pp_bvalue v1 pp_bvalue v2;
    Vunknown
  | _, _, _                  ->
    Glob_option.print_full "@[<v>eeq: cannot evaluate Oeq %a %a@]@."
      pp_bvalue v1 pp_bvalue v2;
    Vunknown

let enamecmp (e1,e2 : expr * expr) : bvalue =
  match e1, e2 with
  | Evar v1, Evar v2 ->
    Vbool (String.equal v1.v_name v2.v_name)
  | _ ->
    Glob_option.print_full "@[<v>enamecmp: cannot evaluate %a@]@."
      pp_e_g (Eop({od=Onamecmp}, [e1;e2]));
    Vunknown

let elt s ws (v1, v2) =
  match ws, v1, v2 with
  | None, Vint i1, Vint i2 -> Vbool (B.lt i1 i2)
  | Some ws, Vint i1, Vint i2 ->
    Vbool (B.lt (to_int_s s ws i1) (to_int_s s ws i2))
  | _ -> Vunknown

let ele s ws (v1, v2) =
  match ws, v1, v2 with
  | None, Vint i1, Vint i2 -> Vbool (B.le i1 i2)
  | Some ws, Vint i1, Vint i2 ->
    Vbool (B.le (to_int_s s ws i1) (to_int_s s ws i2))
  | _ -> Vunknown

let esignextend ws1 ws2 v =
  match v with
  | Vint i -> Vint (of_int ws2 (to_int ws1 i))
  | _      -> Vunknown

let ezeroextend ws1 ws2 v =
  match v with
  | Vint i -> Vint (of_int ws2 (to_uint ws1 i))
  | _      -> Vunknown

let ecast_w ws v =
  match v with
  | Vint i -> Vint (of_int ws i)
  | Vptr _ -> v (* FIXME Vptr { p with p_ofs = of_int ws p.p_ofs } *)
  | _      -> Vunknown

let ecast_int s bty v =
  match v, bty, s with
  | Vint i, W w, Signed   -> Vint (to_int w i)
  | Vint i, W w, Unsigned -> Vint (to_uint w i)
  | Vint i, Int, Signed   -> Vint i
  | Vint i, Int, Unsigned -> Vint (B.abs i)
  | Vbool b, Bool    , _        -> if b then Vint B.one else Vint B.zero
  | Vptr p, W w, Signed   -> Vptr {p with p_ofs = to_int w p.p_ofs}
  | Vptr p, W w, Unsigned -> Vptr {p with p_ofs = to_uint w p.p_ofs}
  | _     , _     , _        -> Vunknown

let eval_op (loc:full_loc) (op:Il.op) (vs:bvalue list) (es:expr list)
  : bvalue =
  match op.od with
  | Oif   _  -> eif (as_seq3 vs)
  | Oadd  ws -> eadd loc ws (as_seq2 vs)
  | Omul  ws -> emul ws (as_seq2 vs)
  | Omulh _ ->
    ev_hierror loc "op %s not yet implemented please raise an issue request" (op_string op)
  | Osub  ws -> esub ws (as_seq2 vs)
  | Oopp  ws -> eopp ws (as_seq1 vs)
  | Olsl  ws -> elsl ws (as_seq2 vs)
  | Olsr  ws -> elsr ws (as_seq2 vs)
  | Oasr  ws -> easr ws (as_seq2 vs)
  | Oand  ws -> eand ws (as_seq2 vs) (as_seq2 es)
  | Oxor  ws -> exor ws (as_seq2 vs) (as_seq2 es)
  | Oor   ws -> eor  ws (as_seq2 vs) (as_seq2 es)
  | Onot  ws -> enot ws (as_seq1 vs)
  | Oeq  bty -> eeq bty (as_seq2 vs) (as_seq2 es)
  | Onamecmp -> enamecmp (as_seq2 es)
  | Olt (s,ws) -> elt s ws (as_seq2 vs)
  | Ole (s,ws) -> ele s ws (as_seq2 vs)
  | Osignextend(ws1,ws2) -> esignextend ws1 ws2 (as_seq1 vs)
  | Ozeroextend(ws1,ws2) -> ezeroextend ws1 ws2 (as_seq1 vs)
  | Ocast_w ws -> ecast_w ws (as_seq1 vs)
  | Ocast_int (s,bty) -> ecast_int s bty (as_seq1 vs)

(* ********************************************** *)
(* Expressions evaluation                         *)

let eval_var st x =
  match Mv.find x st.st_mvar with
  | Vbase v -> v
  | Varr _ -> Vunknown
  | exception Not_found -> Vunknown

let eval_index (loc:full_loc) msg x i =
  let _, i1, i2 = get_arr x.v_ty in
  if not (B.le i1 i && B.le i i2) then
    ev_hierror loc "%s out of bound (%a:[%a:%a]) [%a] " msg
      V.pp_g x B.pp_print i1 B.pp_print i2 B.pp_print i;
  B.to_int (B.sub i i1)

let eval_get (loc:full_loc) st x (v,ei) =
  match v with
  | Vint i ->
    let ofs = eval_index loc "eval_get" x i in
    let vi =
      match Mv.find x st.st_mvar with
      | Varr t              -> t.(ofs)
      | Vbase Vunknown      -> Vunknown
      | exception Not_found -> Vunknown
      | _                   -> assert false in
    vi, Eget(x,Eint i)
  | _ ->
    Vunknown, Eget(x, ei)

let get_ofs loc ws p =
  let q = B.of_int (ws_byte ws) in
  (* pointer's offset is a multiple of the expected word-size *)
  if not (B.equal (B.erem p.p_ofs q) B.zero) then
    ev_hierror loc "illegal access: \
                    pointer's offset %a is not a multiple of the expected word-size %a@."
      B.pp_print p.p_ofs pp_wsize ws;
  B.div p.p_ofs q

let eval_mem_index (loc:full_loc) (st:state) (ws:Common.wsize) (m:var) (e:expr) (v,_ei)
  : bvalue array * int * var * expr =
  match v with
  | Vptr p when V.equal m p.p_mem ->
    begin
      (* get type and bounds of destination  *)
      let bty, _i1, _i2 = get_arr p.p_dest.v_ty in
      (* get word-size of destination *)
      let dst_ws = get_ws bty in
      (* match on kind of access *)
      match ws_eq dst_ws ws, ws_le dst_ws ws with
      | true, _ -> (* accessing equal word-size *)
        (* compute offset in multiples of destination's word-size *)
        let ofs = get_ofs loc dst_ws p in
        (* compute relative offset to destinations lower bound *)
        let iofs = eval_index loc "eval_load region" p.p_dest ofs in
        (* get array holding values from state *)
        let t =
          try Mv.find p.p_dest st.st_mregion
          with Not_found ->
            ev_hierror loc "%a@ eval_mem_index: unknown region %a"
              pp_state st V.pp_dbg p.p_dest in
        t, iofs, p.p_dest, Eint ofs
      | false, true -> (* accessing multiple words of destination at once *)
        (* FIXME Benjamin: implement the projection *)
        ev_hierror loc "%a@ need to implement access %a to \
                        destination %a with smaller word-size %a than access word-size %a"
          pp_state st pp_bvalue v V.pp_dbg p.p_dest pp_wsize dst_ws pp_wsize ws
      | false, false -> (* trying to access a fraction of destination word-size *)
        ev_hierror loc "%a@ eval_mem_index: invalid access %a to \
                        destination %a with word-size %a smaller than access of word-size %a"
          pp_state st pp_bvalue v V.pp_dbg p.p_dest pp_wsize dst_ws pp_wsize ws
    end
  | _ ->
    ev_hierror loc "@[<v>%a@ eval_mem_index: cannot evaluate pointer %a in %a@]"
      pp_state st pp_bvalue v
      (pp_e ~full:!Glob_option.full) e

let eval_load loc st ws m e (v,ei) : bvalue * expr =
  let t, iofs, dest, eofs = eval_mem_index loc st ws m e (v,ei) in
  t.(iofs), Eget(dest, eofs)

let rec eval_e (loc:full_loc) (st:state) (e:expr) : bvalue * expr =
  match e with
  | Eint i  -> Vint i, e
  | Ebool b -> Vbool b, e
  | Evar x  -> eval_var st x, e
  | Eget(x,e) -> eval_get loc st x (eval_e loc st e)
  | Eload(ws, m, e) -> eval_load loc st ws m e (eval_e loc st e)
  | Eop(op, es) ->
    let vs, es = eval_es loc st es in
    let v = eval_op loc op vs es in
    let e =
      match v with
      | Vint i  -> Eint i
      | Vbool b -> Ebool b
      | _       -> Eop(op, es) in
    v, e

and eval_es (loc:full_loc) (st:state) (es:expr list) =
  List.split (List.map (eval_e loc  st) es)

(* ********************************************* *)
(* Programs evaluation                           *)

let find_label (loc:full_loc) (lbl:Lbl.t) (st:state) : instr list option =
  let rec aux c =
    match c with
    | [] -> None
    | i :: c' ->
      match i.i_desc with
      | Iif(_,c1,c2) | Iwhile(c1,_,c2) ->
        begin
          try aux c'
          with Not_found ->
            try aux c1
            with Not_found ->
              aux c2
        end
      | Ilabel lbl' when Lbl.equal lbl lbl' ->
        Some c'
      | Ilabel _
      | Iassgn _
      | Ileak _
      | Imacro _
      | Igoto _
      | Iigoto _ ->
        aux c'
  in
  aux st.st_prog

let unknown_arr i1 i2 =
  assert (B.le i1 i2);
  let size = B.to_int (B.add (B.sub i2 i1) B.one) in
  Array.make size Vunknown

let rec eval_i (st:state) : unit =
(*  Format.eprintf "%a@." pp_state st; *)
  match st.st_pc with
  | [] -> ()
  | i :: c ->
    match i.i_desc with
    | Iassgn (x, e) -> eval_assgn i.i_loc st x e c
    | Ileak(li, es) ->
      let i' = { i_desc = Ileak(li, snd (eval_es i.i_loc st es)); i_loc = i.i_loc } in
      next st (Some i') c
    | Imacro (mname,_) ->
      (* TODO implement this one as well *)
      ev_hierror i.i_loc "@[<v>%a@ eval %a: found macro %s but expected it to be inlined@]"
        pp_state st pp_i_dbg i mname
    | Ilabel _ ->
      next st (Some i) c
    | Igoto lbl ->
      (match find_label i.i_loc lbl st with
      | Some c -> next st (None) c
      | None ->
        begin
          try
            (* check that label points to a macro *)
            let mn = Ml.find lbl st.st_global.glob_lbl in
            let m = Ms.find mn st.st_global.macro in
            (* check that macro has no parameters *)
            (* FIXME WARNING goto correct position in macro *)
            if List.length m.mc_params != 0 then
              ev_hierror i.i_loc "@[<v>%a@ eval Igoto: global jump to macro %s with arguments \
                                  but expected it to be inlined@]"
                pp_state st mn pp_i_dbg i;
            (* change body of current evaluation *)
            st.st_prog <- m.mc_body;
            next st (None) m.mc_body
              (* on return take evaluated part and append, proceed with next instr. *)
          with Not_found ->
            ev_hierror i.i_loc "@[<v>%a@ %a@ eval Igoto: encountered global jump \"%a\" \
                                with unknown label@]"
              pp_state st pp_genv_dbg st.st_global pp_i_dbg i
          end)
    | Iigoto x ->
      begin match eval_var st x with
      | Vcptr lbl ->
        (match find_label i.i_loc lbl st with
         | Some c -> next st (None) c
         | None ->
           ev_hierror i.i_loc "@[<v>%a@ eval Iigoto: encountered global jump %a \
                               but expected it to be inlined@]"
             pp_state st pp_i_dbg i)
      | bv ->
        ev_hierror i.i_loc "@[<v>%a@ eval unexpected value in Iigoto: %a @ \
                            expected a pointer but got %a@]"
          pp_state st pp_i_dbg i pp_bvalue bv
      end
    | Iif(e,c1,c2) ->
      begin match eval_e i.i_loc st e with
      | Vbool b, _  -> next st None ((if b then c1 else c2) @ c)
      | Vunknown, _ ->
        ev_hierror i.i_loc "@[<v>%a@ eval Iif: cannot evaluate conditional expression \"%a\" at %a@]"
          pp_state st
          pp_e_dbg e
          pp_full_loc i.i_loc
      | _, _        -> assert false
      end
    | Iwhile (c1, e, c2) ->
      let c = c1 @ {i_desc = Iif (e, c2 @ [i], []); i_loc = i.i_loc} :: c in
      next st None c

and next (st:state) (i:Il.instr option) (c:Il.cmd) : unit =
  oiter (fun i -> st.st_eprog <- i :: st.st_eprog) i;
  st.st_pc <- c;
  eval_i st

and eval_assgn (loc:Utils.full_loc) (st:state) (lv:Il.lval) (e:Il.expr) (c:Il.cmd) =
  let v, e = eval_e loc st e in
  let lv, c =
    match lv with
    | Lvar x ->
      st.st_mvar <- Mv.add x (Vbase v) st.st_mvar;
      if x.v_name = "pc" then
        lv, {i_desc = Iigoto x; i_loc = loc}::c
      else
        lv, c
    | Lset(x,ei) ->
      let vi, ei = eval_e loc st ei in
      begin match vi with
      | Vint i   ->
        let _, i1, i2 = get_arr x.v_ty in
        if not (B.le i1 i && B.le i i2) then
          ev_hierror loc "eval_set : out of bound";
        let ofs = B.to_int (B.sub i i1) in
        let t =
          match Mv.find x st.st_mvar with
          | Varr t              -> t
          | Vbase Vunknown      -> unknown_arr i1 i2
          | exception Not_found -> unknown_arr i1 i2
          | _                   -> assert false in
        t.(ofs) <- v;
        st.st_mvar <- Mv.add x (Varr t) st.st_mvar

      | Vunknown ->
        st.st_mvar <- Mv.remove x st.st_mvar

      | _        -> assert false
      end;
      Lset(x,ei), c

    | Lstore(ws, m, ei) ->
      let t, iofs, dest, eofs = eval_mem_index loc st ws m ei (eval_e loc st ei) in
      t.(iofs) <- v;
      st.st_mregion <- Mv.add dest t st.st_mregion;
      Glob_option.print_full "@[ileval: updating memory %a: %a gets %a@]@."
        V.pp_g m V.pp_g dest (pp_list ", " pp_bvalue) (Array.to_list t);
      Lset(dest, eofs), c
  in
  next st (Some {i_desc = Iassgn(lv, e); i_loc = loc }) c

let empty_eenv = {
  state     = Ms.empty;
  initial   = Ms.empty;
}

let find_initial eenv mn =
  try
    Ms.find mn eenv.initial
  with Not_found ->
    hierror "evaluation" None "no initial state annotation available for macro %s" mn

let find_state eenv mn =
  try
    Ms.find mn eenv.state
  with Not_found ->
    hierror "evaluation" None "no program state available for macro %s" mn

let update_initial eenv mn initial =
  try
    { eenv with initial = Ms.update mn mn initial eenv.initial }
  with Not_found ->
    { eenv with initial = Ms.add mn initial eenv.initial }

let update_state eenv mn state =
  try
    { eenv with state = Ms.update mn mn state eenv.state }
  with Not_found ->
    { eenv with state = Ms.add mn state eenv.state }

let partial_eval (g:genv) (eenv:eenv) (m:Il.macro) =
  let mdest = ref Mv.empty in

  let init = find_initial eenv m.mc_name in

  let init_region mr r =
    assert (r.r_from.v_ty = Tmem);
    let _, i1, i2 = get_arr r.r_dest.v_ty in
    let t = unknown_arr i1 i2 in
    assert (not (Mv.mem r.r_dest mr));
    mdest := Mv.add r.r_dest r.r_from !mdest;
    Mv.add r.r_dest t mr in
  let st_mregion = List.fold_left init_region Mv.empty init.init_region in

  let init_var (mv,mr:(value Mv.t) * (bvalue array Mv.t)) (x, iv) =
    let map v =
      match v with
      | Iint    i  -> Vint i
      | Ibool   b  -> Vbool b
      | Ilbl    l  -> Vcptr l
      | Icptr_exit -> Vcptr Lbl.exit_
      | Iarr _
      | Iptr _     -> assert false in
    let v =
      match iv with
      | Iint _
      | Ibool _
      | Ilbl _
      | Icptr_exit -> Vbase (map iv)
      | Iarr iv -> Varr (Array.of_list (List.map map iv))
      | Iptr(d, ofs) ->
        let m =
          try Mv.find d !mdest with Not_found -> assert false in
        Vbase (Vptr { p_mem = m; p_dest = d; p_ofs = ofs }) in
    (* decide whether to update a state variable or a variable in memory *)
    match Mv.Exceptionless.find x mr, v with
    | Some mvar, Varr ba ->
      Glob_option.print_full "replacing region %a of %a by %a@."
        V.pp_g x (pp_list ", " pp_bvalue) (Array.to_list mvar)
        (pp_list ", " pp_bvalue) (Array.to_list ba);
      mv, Mv.add x ba mr
    | None, Vbase _ ->
      Mv.add x v mv, mr
    | _, _ ->
      assert false
  in
  let st_mvar, st_mregion = List.fold_left init_var (Mv.empty, st_mregion) init.init_var in
  (* TODO set the program counter accordingly *)

  Glob_option.print_full "DEBUG mc@ @[<v>%a@]@." (pp_cmd ~full:true) m.mc_body;
  let st = {
    st_mregion;
    st_mvar;
    st_prog  =
      m.mc_body @
      [ { i_desc = Ilabel Lbl.exit_; i_loc = dummy_full_loc; } ];
    st_pc    = m.mc_body;
    st_eprog = [];
    st_global = g;
    } in
  Glob_option.print_full "DEBUG st@ @[<v>%a@]@." (pp_cmd ~full:true) st.st_prog;

  eval_i st;
  { st with st_eprog = List.rev st.st_eprog }
