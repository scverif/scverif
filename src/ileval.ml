open Utils
open Common
open Il

let ev_hierror () = hierror "evaluation" None

type pointer =
  { p_mem  : V.t;
    p_dest : V.t;
    p_ofs  : B.zint }

type cpointer = Lbl.t

type bvalue =
  | Vcptr of Lbl.t
  | Vptr  of pointer
  | Vint  of B.zint
  | Vbool of bool
  | Vunknown

type value =
  | Varr  of bvalue array
  | Vbase of bvalue

type state = {
    mutable st_mregion : bvalue array Mv.t;
    mutable st_mvar    : value Mv.t;
            st_prog    : cmd;
    mutable st_pc      : cmd;
    mutable st_eprog   : cmd;
  }

module Ptr = struct
  type t = pointer
  let equal p1 p2 =
    V.equal p1.p_mem  p2.p_mem  &&
    V.equal p1.p_dest p2.p_dest &&
    B.equal p1.p_ofs  p2.p_ofs

  let pp fmt p =
    Format.fprintf fmt "[%a @@ %a %a]"
      V.pp_dbg p.p_mem
      V.pp_dbg p.p_dest
      B.pp_print p.p_ofs
end

(* Pretty printer *)

let pp_bvalue fmt = function
  | Vcptr lbl -> Format.fprintf fmt "lbl %a" Lbl.pp_dbg lbl
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
           V.pp_dbg x
           (pp_list ",@ " pp_bvalue) (Array.to_list t)) mr;
  Format.fprintf fmt "@]"

let pp_vars fmt mv =
  Format.fprintf fmt "  @[<v>";
  Mv.iter (fun x v ->
      Format.fprintf fmt "%a -> %a@ "
           V.pp_dbg x pp_value v) mv;
  Format.fprintf fmt "@]"

let pp_state fmt st =
  Format.fprintf fmt "@[<v>regions:@ %a@ vars:@ %a@ pc:@ %a@ eprog:@ %a@]"
    pp_regions st.st_mregion
    pp_vars    st.st_mvar
    (pp_cmd ~full:true) st.st_pc
    (pp_cmd ~full:true) (List.rev st.st_eprog)

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

let eadd ws (v1, v2) =
  match ws, v1, v2 with
  | None   , Vint i1, Vint i2 -> Vint (B.add i1 i2)
  | _      , Vptr p , Vint i
  | _      , Vint i , Vptr p  -> Vptr { p with p_ofs = B.add p.p_ofs i }
  | Some ws, Vint i1, Vint i2 -> op_ww_w B.add ws i1 i2
  | _                         -> Vunknown

let esub ws (v1, v2) =
  match ws, v1, v2 with
  | None   , Vint i1, Vint i2 -> Vint (B.sub i1 i2)
  | _      , Vptr p , Vint i
  | _      , Vint i , Vptr p  -> Vptr { p with p_ofs = B.sub p.p_ofs i }
  | Some ws, Vint i1, Vint i2 -> op_ww_w B.sub ws i1 i2
  | _                         -> Vunknown

let eopp ws v1 =
  match ws, v1 with
  | None   , Vint i -> Vint (B.neg i)
  | Some ws, Vint i -> op_w_w B.neg ws i
  | _               -> Vunknown

let emul ws (v1,v2) =
  match ws, v1, v2 with
  | None, Vint i1, Vint i2    -> Vint (B.mul i1 i2)
  | Some ws, Vint i1, Vint i2 -> op_ww_w B.mul ws i1 i2
  | _                         -> Vunknown

let elsl ws (v1, v2) =
  match v1, v2 with
  | Vint i1 , Vint i2  -> op_wi_w B.lshl ws i1 i2
  | _                  -> Vunknown

let elsr ws (v1, v2) =
  match v1, v2 with
  | Vint i1 , Vint i2  -> op_wi_w B.lshr ws i1 i2
  | _                  -> Vunknown

let easr ws (v1, v2) =
  match v1, v2 with
  | Vint i1 , Vint i2  -> op_wi_w B.ashr ws i1 i2 (* Check this *)
  | _                  -> Vunknown

let eand ws (v1, v2) =
  match ws, v1, v2 with
  | None   , Vbool b1, Vbool b2 -> Vbool (b1 && b2)
  | Some ws, Vint i1 , Vint i2  -> op_ww_w B.lgand ws i1 i2
  | _                           -> Vunknown

let exor ws (v1, v2) =
  match ws, v1, v2 with
  | None   , Vbool b1, Vbool b2 -> Vbool (if b1 then not b2 else b2)
  | Some ws, Vint i1 , Vint i2  -> op_ww_w B.lgxor ws i1 i2
  | _                           -> Vunknown

let eor ws (v1, v2) =
  match ws, v1, v2 with
  | None   , Vbool b1, Vbool b2 -> Vbool (b1 || b2)
  | Some ws, Vint i1 , Vint i2  -> op_ww_w B.lgor ws i1 i2
  | _                           -> Vunknown

let enot ws v =
  match ws, v with
  | None   , Vbool b -> Vbool (not b)
  | Some ws, Vint i  -> op_w_w B.lgnot ws i
  | _                ->
    Format.printf "@[<v>enot: cannot evaluate Onot %a@]"
      pp_bvalue v;
    Vunknown


let eeq bty (v1,v2) =
  match bty, v1, v2 with
  | Int , Vint  i1, Vint  i2 -> Vbool (B.equal i1 i2)
  | _   , Vptr  p1, Vptr  p2 -> Vbool (Ptr.equal p1 p2)
  | Bool, Vbool b1, Vbool b2 -> Vbool (b1 = b2)
  | W ws, Vint  i1, Vint  i2 -> Vbool (B.equal (of_int ws i1) (of_int ws i2))
  | _, _, _                  ->
    Format.printf "@[<v>eeq: cannot evaluate Oeq %a %a@]"
      pp_bvalue v1 pp_bvalue v2;
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
  | Vptr _ -> v (* Vptr { p with p_ofs = of_int ws p.p_ofs } *)
  | _      -> Vunknown

let ecast_int s ws v =
  match v, ws, s with
  | Vint i, Some w, Signed   -> Vint (to_int w i)
  | Vint i, Some w, Unsigned -> Vint (to_uint w i)
  | Vint i, None  , Signed   -> Vint i
  | Vint i, None  , Unsigned -> Vint (B.abs i)
  | Vbool b, _    , _        -> if b then Vint B.one else Vint B.zero
  | Vptr p, Some w, Signed   -> Vptr {p with p_ofs = to_int w p.p_ofs}
  | Vptr p, Some w, Unsigned -> Vptr {p with p_ofs = to_uint w p.p_ofs}

  | _     , _     , _        -> Vunknown

let eval_op op vs =
  match op.od with
  | Oif   _  -> eif (as_seq3 vs)
  | Oadd  ws -> eadd ws (as_seq2 vs)
  | Omul  ws -> emul ws (as_seq2 vs)
  | Omulh _ ->
    ev_hierror () "op %s not yet implemented please report" (op_string op)
  | Osub  ws -> esub ws (as_seq2 vs)
  | Oopp  ws -> eopp ws (as_seq1 vs)
  | Olsl  ws -> elsl ws (as_seq2 vs)
  | Olsr  ws -> elsr ws (as_seq2 vs)
  | Oasr  ws -> easr ws (as_seq2 vs)
  | Oand  ws -> eand ws (as_seq2 vs)
  | Oxor  ws -> exor ws (as_seq2 vs)
  | Oor   ws -> eor  ws (as_seq2 vs)
  | Onot  ws -> enot ws (as_seq1 vs)
  | Oeq  bty -> eeq bty (as_seq2 vs)
  | Olt (s,ws) -> elt s ws (as_seq2 vs)
  | Ole (s,ws) -> ele s ws (as_seq2 vs)
  | Osignextend(ws1,ws2) -> esignextend ws1 ws2 (as_seq1 vs)
  | Ozeroextend(ws1,ws2) -> ezeroextend ws1 ws2 (as_seq1 vs)
  | Ocast_w ws -> ecast_w ws (as_seq1 vs)
  | Ocast_int (s,ws) -> ecast_int s ws (as_seq1 vs)

(* ********************************************** *)
(* Expressions evaluation                         *)

let eval_var st x =
  match Mv.find x st.st_mvar with
  | Vbase v -> v
  | Varr _ -> Vunknown
  | exception Not_found -> Vunknown

let eval_index msg x i =
  let _, i1, i2 = get_arr x.v_ty in
  if not (B.le i1 i && B.le i i2) then
    ev_hierror () "%s out of bound (%a:[%a:%a]) [%a] " msg
      V.pp_g x B.pp_print i1 B.pp_print i2 B.pp_print i;
  B.to_int (B.sub i i1)

let eval_get st x (v,ei) =
  match v with
  | Vint i ->
    let ofs = eval_index "eval_get" x i in
    let vi =
      match Mv.find x st.st_mvar with
      | Varr t              -> t.(ofs)
      | Vbase Vunknown      -> Vunknown
      | exception Not_found -> Vunknown
      | _                   -> assert false in
    vi, Eget(x,Eint i)
  | _ ->
    Vunknown, Eget(x, ei)

let get_ofs ws p =
  let q = B.of_int (ws_byte ws) in
  assert (B.equal (B.erem p.p_ofs q) B.zero);
  B.div p.p_ofs q

let eval_mem_index st ws m e (v,_ei) =
  match v with
  | Vptr p when V.equal m p.p_mem ->
    let bty, _i1, _i2 = get_arr p.p_dest.v_ty in
    if bty <> W ws then
      ev_hierror () "eval_mem_index : invalid word size";
    let ofs = get_ofs ws p in
    let iofs = eval_index "eval_load region" p.p_dest ofs in
    let t =
      try Mv.find p.p_dest st.st_mregion
      with Not_found ->
        ev_hierror () "eval_mem_index : unknown region" in
    t, iofs, p.p_dest, Eint ofs
  | _ ->
    ev_hierror () "@[<v>%a@ eval_mem_index : cannot evaluate pointer %a@]"
      pp_state st
      (pp_e ~full:!Glob_option.full) e

let eval_load st ws m e (v,ei) =
  let t, iofs, dest, eofs = eval_mem_index st ws m e (v,ei) in
  t.(iofs), Eget(dest, eofs)

let rec eval_e st e =
  match e with
  | Eint i  -> Vint i, e
  | Ebool b -> Vbool b, e
  | Evar x  -> eval_var st x, e
  | Eget(x,e) -> eval_get st x (eval_e st e)
  | Eload(ws, m, e) -> eval_load st ws m e (eval_e st e)
  | Eop(op, es) ->
    let vs, es = eval_es st es in
    let v = eval_op op vs in
    let e =
      match v with
      | Vint i  -> Eint i
      | Vbool b -> Ebool b
      | _       -> Eop(op, es) in
    v, e

and eval_es st es = List.split (List.map (eval_e st) es)

(* ********************************************* *)
(* Programs evaluation                           *)

let find_label lbl st =
  let rec aux c =
    match c with
    | [] -> raise Not_found
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
  | Ilabel lbl' when Lbl.equal lbl lbl' -> c'
  | (Ilabel _ | Iassgn _ | Ileak _ | Imacro _ | Igoto _ | Iigoto _) ->
    aux c' in
  try aux st.st_prog
  with Not_found ->
    ev_hierror () "unknown label %a" Lbl.pp_dbg lbl

let unknown_arr i1 i2 =
  assert (B.le i1 i2);
  let size = B.to_int (B.add (B.sub i2 i1) B.one) in
  Array.make size Vunknown

let rec eval_i st =
(*  Format.eprintf "%a@." pp_state st; *)
  match st.st_pc with
  | [] -> ()
  | i :: c ->
    match i.i_desc with
    | Iassgn (x, e) -> eval_assgn st i.i_loc x e c
    | Ileak(li, es) ->
      let i' = { i_desc = Ileak(li, snd (eval_es st es)); i_loc = i.i_loc } in
      next st (Some i') c
    | Imacro (m,_) ->
      ev_hierror () "@[<v>%a@ eval Imacro: found macro %s but expected it to be inlined]"
        pp_state st
        m.mc_name
    | Ilabel _ ->
      next st (Some i) c
    | Igoto lbl ->
      let c = find_label lbl st in
      next st (Some i) c
    | Iigoto x ->
      begin match eval_var st x with
      | Vcptr lbl ->
        let c = find_label lbl st in
        next st (Some i) c
      | _ -> assert false (* FIXME : error msg *)
      end
    | Iif(e,c1,c2) ->
      begin match eval_e st e with
      | Vbool b, _  -> next st None ((if b then c1 else c2) @ c)
      | Vunknown, _ ->
        ev_hierror () "@[<v>%a@ eval Iif: cannot evaluate conditional expression %a@]"
          pp_state st
          (pp_e ~full:!Glob_option.full) e
      | _, _        -> assert false
      end
    | Iwhile (c1, e, c2) ->
      let c = c1 @ {i_desc = Iif (e, c2 @ [i], []); i_loc = i.i_loc} :: c in
      next st None c

and next st i c =
  oiter (fun i ->  st.st_eprog <- i :: st.st_eprog) i;
  st. st_pc <- c;
  eval_i st

and eval_assgn st loc lv e c =
  let v, e = eval_e st e in
  let lv, c =
    match lv with
    | Lvar x ->
      st.st_mvar <- Mv.add x (Vbase v) st.st_mvar;
      if x.v_name = "pc" then
        lv, {i_desc = Iigoto x; i_loc = loc}::c
      else
        lv, c
    | Lset(x,ei) ->
      let vi, ei = eval_e st ei in
      begin match vi with
      | Vint i   ->
        let _, i1, i2 = get_arr x.v_ty in
        if not (B.le i1 i && B.le i i2) then
          ev_hierror () "eval_set : out of bound";
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
      let t, iofs, dest, eofs = eval_mem_index st ws m ei (eval_e st ei) in
      t.(iofs) <- v;
      Lset(dest, eofs), c
  in
  next st (Some {i_desc = Iassgn(lv, e); i_loc = loc }) c

type region = {
    r_from  : V.t;    (* name of the memory *)
    r_dest  : V.t;    (* variable destination *)
  }

type ival =
  | Iint       of B.zint
  | Ibool      of bool
  | Iregion    of V.t * B.zint
  | Icptr_exit

type initial = {
    init_region : region list;
    init_var    : (V.t * ival) list;
  }

let partial_eval init m =
  let mdest = ref Mv.empty in

  let init_region mr r =
    assert (r.r_from.v_ty = Tmem);
    let _, i1, i2 = get_arr r.r_dest.v_ty in
    let t = unknown_arr i1 i2 in
    assert (not (Mv.mem r.r_dest mr));
    mdest := Mv.add r.r_dest r.r_from !mdest;
    Mv.add r.r_dest t mr in
  let st_mregion = List.fold_left init_region Mv.empty init.init_region in

  let init_var mv (x, iv) =
    let v =
      match iv with
      | Iint    i       -> Vint i
      | Ibool   b       -> Vbool b
      | Iregion (d,ofs) ->
        let m =
          try Mv.find d !mdest with Not_found -> assert false in
        Vptr { p_mem = m; p_dest = d; p_ofs = ofs }
      | Icptr_exit      -> Vcptr Lbl.exit_ in
    Mv.add x (Vbase v) mv in
  let st_mvar = List.fold_left init_var Mv.empty init.init_var in

  let st = {
    st_mregion;
    st_mvar;
    st_prog  =
      m.mc_body @
      [ { i_desc = Ilabel Lbl.exit_; i_loc = dummy_full_loc; } ];
    st_pc    = m.mc_body;
    st_eprog = []
    } in

  eval_i st;

  List.rev st.st_eprog
