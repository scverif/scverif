(* Copyright 2019-2020 - NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)

open Utils
open Common
open Location

(* Types as in scv.mli *)

type scvstring    = [%import: Scv.scvstring]
type scvval       = [%import: Scv.scvval]
type scvtarget    = [%import: Scv.scvtarget]
type scvcheckkind = [%import: Scv.scvcheckkind]
type scvmvrewriteparam = [%import: Scv.scvmvrewriteparam]
type scvprintkind = [%import: Scv.scvprintkind]
type scvverbosity = [%import: Scv.scvverbosity]
type scvcmd       = [%import: Scv.scvcmd]


(* Pretty printers *)
let pp_scvstring fmt s =
  Format.fprintf fmt "%a" pp_string (unloc s)

let rec pp_scvval fmt v =
  if !Glob_option.full then
    (match v with
     | SCVNull -> Format.fprintf fmt "null"
     | SCVBool b -> Format.fprintf fmt "%b" b
     | SCVInt i -> Format.fprintf fmt "%a" B.pp_print i
     | SCVString s -> Format.fprintf fmt "%s" (unloc s)
     | SCVList l ->
       Format.fprintf fmt "list[%a]"
         (pp_list ", " (pp_scvval)) l
     | SCVRecord a ->
       let pp_scvrecelem fmt (k,v) =
         Format.fprintf fmt "%a: %a"
           pp_scvstring k
           pp_scvval v in
       Format.fprintf fmt "record(@[<v>%a@])"
         (pp_list "@ " pp_scvrecelem) a
     | SCVMap(i, []) ->
       Format.fprintf fmt "map(@[<v>%a: noargs@])"
         pp_scvstring i
     | SCVMap(i, vs) ->
       Format.fprintf fmt "map(@[<v>%a:@   @[<v>%a@]@])"
         pp_scvstring i
         (pp_list "@ " pp_scvval) vs)
  else
    (match v with
     | SCVNull -> Format.fprintf fmt ""
     | SCVBool b -> Format.fprintf fmt "%b" b
     | SCVInt i -> Format.fprintf fmt "%a" B.pp_print i
     | SCVString s -> Format.fprintf fmt "%s" (unloc s)
     | SCVList l ->
       Format.fprintf fmt "[%a]"
         (pp_list ", " (pp_scvval)) l
     | SCVRecord a ->
       let pp_scvrecelem fmt (k,v) =
         Format.fprintf fmt "%a: %a"
           pp_scvstring k
           pp_scvval v in
       Format.fprintf fmt "@[<v>%a@]"
         (pp_list "@ " pp_scvrecelem) a
     | SCVMap(i, []) ->
       Format.fprintf fmt "@[<v>%a: noargs@];"
         pp_scvstring i
     | SCVMap(i, vs) ->
       Format.fprintf fmt "@[<v>%a:@   @[<v>%a@]@];"
         pp_scvstring i
         (pp_list "@ " pp_scvval) vs)

let pp_scvtarget fmt t =
  match t with
  | TIdent(s) ->
    Format.fprintf fmt "[@[<v>%a@]]"
      (pp_list ",@," (fun fmt s -> pp_string fmt (unloc s))) s
  | TRegex(r) ->
    Format.fprintf fmt "\"%s\""
      (unloc r)
  | TWildcard _ ->
    Format.fprintf fmt "\"*\""

let pp_scvcheckkind fmt ca =
  match ca with
  | Noninterference ->
    Format.fprintf fmt "NI"
  | Strongnoninterference ->
    Format.fprintf fmt "SNI"
  | StatefulNoninterference ->
    Format.fprintf fmt "Stateful NI"
  | StatefulStrongnoninterference ->
    Format.fprintf fmt "Stateful SNI"

let pp_scvprintkind fmt p =
  match p with
  | PMacro ->
    Format.fprintf fmt "macro"
  | PGenv ->
    Format.fprintf fmt "globals"
  | PState ->
    Format.fprintf fmt "state"
  | PInitialEnvironment ->
    Format.fprintf fmt "env"
  | PEvaluatedTrace ->
    Format.fprintf fmt "trace"
  | PMaskverifProg ->
    Format.fprintf fmt "maskverif"
  | PVariableValue _ ->
    Format.fprintf fmt "variables"

let sym_inferpubin = "inferinput"
let sym_inferstout = "inferoutput"

let pp_scvmvrewriteparam fmt p =
  Format.fprintf fmt "@[<v>%s: %b@ %s: %b@]"
    sym_inferpubin p.inferpubin
    sym_inferstout p.inferstout

let pp_scvverbosity fmt v =
  Format.fprintf fmt "%d" (unloc v)

let pp_scvcmd fmt c =
  match c with
  | Accumulate (t, l, k) ->
    Format.fprintf fmt "@[<v>accumulate:@   @[<v>target: %a@ leaks: %a@ keep: %b@]@]"
      pp_scvtarget t pp_scvtarget l k
  | AddLeakCalls t ->
    Format.fprintf fmt "@[<v>addleakage:@   @[<v>target: %a@]@]"
      pp_scvtarget t
  | DeadCodeElim t ->
    Format.fprintf fmt "@[<v>deadcodeelim:@   @[<v>target: %a@]@]"
      pp_scvtarget t
  | FilterLeakage (t, l, k) ->
    Format.fprintf fmt "@[<v>filterleak:@   @[<v>target: %a@ leaks: %a@ inverse: %b@]@]"
      pp_scvtarget t pp_scvtarget l k
  | InlineMacros t ->
    Format.fprintf fmt "@[<v>inlinecalls:@   @[<v>target: %a@]@]"
      pp_scvtarget t
  | PartialEval t ->
    Format.fprintf fmt "@[<v>partialeval:@   @[<v>target: %a@]@]"
      pp_scvtarget t
  | Print(t, PVariableValue vs, None) ->
    Format.fprintf fmt "@[<v>print:@   @[<v>kind: %a@ target: %a@ variables: %a@]@]"
      pp_scvprintkind (PVariableValue vs)
      pp_scvtarget t
      pp_scvtarget vs
  | Print(t, PVariableValue vs, Some v) ->
    Format.fprintf fmt "@[<v>print:@   @[<v>kind: %a@ target: %a@ variables: %a@ verbosity: %a@]@]"
      pp_scvprintkind (PVariableValue vs)
      pp_scvtarget t
      pp_scvtarget vs
      pp_scvverbosity v
  | Print(t, pk, Some v) ->
    Format.fprintf fmt "@[<v>print:@   @[<v>kind: %a@ target: %a@ verbosity: %a@]@]"
      pp_scvprintkind pk
      pp_scvtarget t
      pp_scvverbosity v
  | Print(t, pk, None) ->
    Format.fprintf fmt "@[<v>print:@   @[<v>kind: %a@ target: %a@]@]"
      pp_scvprintkind pk
      pp_scvtarget t
  | Check(t,ca) ->
    Format.fprintf fmt "@[<v>check:@   @[<v>kind: %a@ target: %a@]@]"
      pp_scvcheckkind ca
      pp_scvtarget t
  | RewriteMV(t, p) ->
    Format.fprintf fmt
      "@[<v>rewriteformv:@   @[<v>target: %a@ %a@]@]"
      pp_scvtarget t pp_scvmvrewriteparam p
  | Verbosity v ->
    Format.fprintf fmt "@[<v>verbosity:@   @[<v>level: %a@]@]"
      pp_scvverbosity v

(* internal exception *)
exception ScvError of Location.t option * string

let scverror loc fmt =
  let buf  = Buffer.create 127 in
  let bfmt = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun _ ->
      Format.pp_print_flush bfmt ();
      let msg = Buffer.contents buf in
      raise (ScvError (loc, msg)))
    bfmt fmt

(* real functions *)
let scvverbosity_to_glob (v:scvverbosity) =
  let i = unloc v in
  if Glob_option.v_full >= i && i >= 0 then
    (i, (Glob_option.v_normal_debug <= i))
  else
    Utils.hierror "Scv.scvverbosity_to_full" (Some(loc v))
      "verbosity %d out of range 0 to %d" i Glob_option.v_full

let extract_scvarg (fieldname:string) (arg:scvval) =
  let c, a =
    (match arg with
    | SCVMap (c, [a]) ->
      c, a
    | SCVRecord([c, a]) ->
      c, a
    | _ ->
      scverror None
        "SCVMap or SCVRecord expected, got: %a"
        pp_scvval arg) in
  if not (String.equal (unloc c) fieldname) then
    scverror (Some (loc c))
      "expected association to %s but got: %a"
      fieldname pp_scvstring c
  else
    c, a

let check_scvarg (args:scvval list) (argnames:string list) =
  if (List.length args) != (List.length argnames) then
    scverror None "expected %d arguments but encountered %d."
      (List.length argnames) (List.length args)
  else
    begin
      let extract_arg (matchs, args) argname =
        begin
          let filter k = List.filter
              (fun (s:scvval) ->
                 (match s with
                  | SCVMap (s, _)
                  | SCVRecord([s,_]) ->
                    if String.equal (unloc s) argname then k
                      else not k
                  | _ -> not k))
              args in
          (* check if the field is present*)
          let matches = filter true in
          match List.length matches with
          | 1 ->
            let ls = filter false in
            Glob_option.print_full
              "debug extract_arg %s:@[<v>@ matchs: %a@ args: %a@ matches: %a@ returnargs: %a@]@."
              argname
              (pp_list ", " pp_scvval) matchs
              (pp_list ", " pp_scvval) args
              (pp_list ", " pp_scvval) matches
              (pp_list ", " pp_scvval) ls;
            (matchs@matches, ls)
          | _ ->
            scverror None
              "expected single occurence of %s in arguments but encountered %d"
              argname (List.length matches)
        end in
      let (matchings, _) = List.fold_left extract_arg ([], args) argnames in
      (* return list of scvvals in the same order as in argnames *)
      matchings
    end

let scvtarget_of_scvval (v:scvval) (fn:string) =
  let c, a =
    (match v with
    | SCVMap (c, a) ->
      c, a
    | SCVRecord([c, a]) ->
      c, [a]
    | _ ->
      Utils.hierror "Scv.scvtarget_of_scvval" None
        "SCVMap or SCVRecord expected, got: %a"
        pp_scvval v) in
  if not (String.equal (unloc c) fn) then
    Utils.hierror "Scv.scvtarget_of_scvval" (Some (loc c))
      "expected fieldname %s but got: %a"
      fn pp_scvstring c
    else
    match a with
    | [SCVList l] ->
      begin
        let ls = List.map
            (function
            | SCVString s -> s
            | e -> Utils.hierror "Scv.scvtarget_of_scvval" None
                     "expected SCVString but got:%a" pp_scvval e)
            l in
        if List.length ls == 1 then
          if String.equal (unloc (List.hd ls)) "any" then
            TWildcard (loc (List.hd ls))
          else
            TIdent ls
        else
          TIdent ls
      end
    | [SCVString s] ->
      if String.equal (unloc s) "any" then
        TWildcard (loc s)
      else
        TRegex(s)
    | _ ->
      Utils.hierror "Scv.scvtarget_of_scvval" (Some (loc c))
        "expected argument of string or list type but got: %a"
        (pp_list ",@," pp_scvval) a

let bool_of_scvval = function
  | SCVRecord[(_,SCVBool b)]
  | SCVBool b -> b
  | e -> scverror None
           "expected SCVBool but got:%a" pp_scvval e

let scvprintkind_of_scvval pk vars =
  let c, a = extract_scvarg "kind" pk in
  match a with
  | SCVString s ->
    (match unloc s with
     | "macrodef" -> PMacro
     | "globals" -> PGenv
     | "state" -> PState
     | "variables" -> PVariableValue (oget vars)
     | "initials" -> PInitialEnvironment
     | "evaltrace" -> PEvaluatedTrace
     | "maskverif" -> PMaskverifProg
     | _ -> scverror (Some (loc s))
              "unknown print kind: %a"
              pp_scvval a
    )
  | _ ->
    scverror (Some (loc c))
      "expected SCVString but got: %a"
      pp_scvval a

let scvverbosity_of_scvval (v:scvval) =
  let c, a = extract_scvarg "verbosity" v in
  match a with
  | SCVInt i ->
    mk_loc (loc c) (B.to_int i)
  | _ ->
    scverror (Some (loc c))
      "expected SCVInt but got: %a"
      pp_scvval a

let scvcheckkind_of_scvval (v:scvval) =
  let c, a = extract_scvarg "kind" v in
  match a with
  | SCVString s ->
    (match unloc s with
     | "NI" -> Noninterference
     | "SNI" -> Strongnoninterference
     | "Stateful NI" -> StatefulNoninterference
     | "Stateful SNI" -> StatefulStrongnoninterference
     | _ -> scverror (Some (loc s))
              "unknown check kind: %a"
              pp_scvval a
    )
  | _ ->
    scverror (Some (loc c))
      "expected SCVString but got: %a"
      pp_scvval a

let scvval_to_scvcmd_loc (v:scvval located) =
  let c, a = match unloc v with
    | SCVMap (c, a) ->
      c, a
    | SCVRecord([c, a]) ->
      c, [a]
    | e ->
      Utils.hierror "Scv.scvval_to_scvcmd_loc" (Some(loc v))
        "SCVMap or SCVRecord expected, got: %a"
        pp_scvval e
  in
  (* TODO: use hashtbl of constructor * scv_to_<cmd> *)
  try (
    match unloc c with
    | "accumulate" ->
      begin
        [@warning "-8"]
        let (t, b, l) =
          match List.length a with
          | 2 -> let [t;b] = check_scvarg a ["target";"keep"] in
            (t, b, TWildcard(loc c))
          | 3 ->
            let [t;b;l] = check_scvarg a ["target";"keep";"leaks"] in
            (t, b, scvtarget_of_scvval l "leaks")
        in
        let target = scvtarget_of_scvval t "target" in
        let keep = bool_of_scvval b in
        mk_loc (loc c) (Accumulate(target, l, keep))
      end
    | "print" ->
      begin
        [@warning "-8"]
        let (k,t,vb,vars) =
          match List.length a with
          | 2 ->
            let [k;t] = check_scvarg a ["kind";"target"] in
            (k,t,None,None)
          | 3 ->
            (try
               let [k;t;vs] = check_scvarg a ["kind";"target";"variables"] in
               (k,t,None,Some (scvtarget_of_scvval vs "variables"))
             with ScvError(_,_) ->
               let [k;t;vb] = check_scvarg a ["kind";"target";"verbosity"] in
               (k,t,Some (scvverbosity_of_scvval vb), None))
          | 4 ->
            let [k;t;vb;vs] = check_scvarg a ["kind";"target";"verbosity";"variables"] in
            (k,t,Some (scvverbosity_of_scvval vb), Some (scvtarget_of_scvval vs "variables"))
        in
        let target = scvtarget_of_scvval t "target" in
        let kind = scvprintkind_of_scvval k vars in
        mk_loc (loc c) (Print(target, kind, vb))
      end
    | "check" ->
      begin
        [@warning "-8"]
        let [k;t] = check_scvarg a ["kind";"target"] in
        let target = scvtarget_of_scvval t "target" in
        let kind = scvcheckkind_of_scvval k in
        mk_loc (loc c) (Check(target, kind))
      end
    | "rewriteformv" ->
      begin
        [@warning "-8"]
        let [t;ipi;iso] = check_scvarg a ["target"; sym_inferpubin; sym_inferstout] in
        let target = scvtarget_of_scvval t "target" in
        let inferpubin = bool_of_scvval ipi in
        let inferstout = bool_of_scvval iso in
        mk_loc (loc c) (RewriteMV(target, {inferpubin; inferstout}))
      end
    | "partialeval" ->
      begin
        [@warning "-8"]
        let [t] = check_scvarg a ["target"] in
        let target = scvtarget_of_scvval t "target" in
        mk_loc (loc c) (PartialEval(target))
      end
    | "inlinecall" ->
      begin
        [@warning "-8"]
        let [t] = check_scvarg a ["target"] in
        let target = scvtarget_of_scvval t "target" in
        mk_loc (loc c) (InlineMacros(target))
      end
    | "deadcodeelim" ->
      begin
        [@warning "-8"]
        let [t] = check_scvarg a ["target"] in
        let target = scvtarget_of_scvval t "target" in
        mk_loc (loc c) (DeadCodeElim(target))
      end
    | "addleakage" ->
      begin
        [@warning "-8"]
        let [t] = check_scvarg a ["target"] in
        let target = scvtarget_of_scvval t "target" in
        mk_loc (loc c) (AddLeakCalls(target))
      end
    | "filterleak" ->
      begin
        [@warning "-8"]
        let (t, l, b) =
          match List.length a with
          | 1 ->
            let [t] = check_scvarg a ["target"] in
            (t, TWildcard (loc c), true)

          | 2 ->
            let [t;b] = check_scvarg a ["target";"inverse"] in
            (t, TWildcard (loc c), bool_of_scvval b)
          | 3 ->
            let [t;l;b] = check_scvarg a ["target";"leaks";"inverse"] in
            let l = scvtarget_of_scvval l "leaks" in
            (t, l, bool_of_scvval b)
        in
        let target = scvtarget_of_scvval t "target" in
        mk_loc (loc c) (FilterLeakage(target, l, b))
      end
    | "verbosity" ->
      begin
        [@warning "-8"]
        let [vb] = check_scvarg a ["verbosity"] in
        let v = scvverbosity_of_scvval vb in
        mk_loc (loc c) (Verbosity(v))
      end
    | _ -> Utils.hierror "Scv.scvval_to_scvcmd_loc" (Some(loc c))
             "unknown constructor %s in %a" (unloc c) pp_scvval (unloc v)
  )
  with
  | ScvError(Some l, msg) ->
    Utils.hierror "Scv.scvval_to_scvcmd_loc" (Some l)
      "@[<v>interpretation of command:@   @[<v>%a@]@ failed with:@   @[<v>%s@]@]" pp_scvval (unloc v) msg
  | ScvError(None, msg) ->
    Utils.hierror "Scv.scvval_to_scvcmd_loc" (Some (loc c))
      "@[<v>interpretation of command:@   @[<v>%a@]@ failed with:@   @[<v>%s@]@]" pp_scvval (unloc v) msg
