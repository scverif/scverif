(* Copyright 2019-2020 - NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)

open Common
open Location

type scvstring = string located

type scvval =
  | SCVNull
  | SCVBool   of bool
  | SCVInt    of B.zint
  | SCVString of scvstring
  | SCVList   of scvval list
  | SCVRecord of (scvstring * scvval) list (* Association list (named arguments) *)
  | SCVMap    of scvstring * scvval list   (* Variant: constructor, arguments *)

type scvtarget =
  | TIdent    of (string located) list
  | TRegex    of string located
  | TWildcard of Location.t

type scvverbosity = int located

type scvprintkind =
  | PMacro
  | PGenv
  | PState
  | PInitialEnvironment
  | PEvaluatedTrace
  | PMaskverifProg
  | PVariableValue of scvtarget

type scvcheckkind =
  | Noninterference
  | Strongnoninterference
  | StatefulNoninterference
  | StatefulStrongnoninterference

type scvmvrewriteparam = {
    inferpubin : bool;
    inferstout : bool;
  }

type scvcmd =
  | Accumulate    of scvtarget * scvtarget * bool
  | AddLeakCalls  of scvtarget
  | DeadCodeElim  of scvtarget
  | FilterLeakage of scvtarget * scvtarget * bool
  | InlineMacros  of scvtarget
  | PartialEval   of scvtarget
  | RewriteMV     of scvtarget * scvmvrewriteparam
  | Print         of scvtarget * scvprintkind * scvverbosity option
  | Check         of scvtarget * scvcheckkind
  | Verbosity     of scvverbosity

val pp_scvstring    : Format.formatter -> scvstring -> unit
val pp_scvval       : Format.formatter -> scvval -> unit
val pp_scvtarget    : Format.formatter -> scvtarget -> unit
val pp_scvcheckkind : Format.formatter -> scvcheckkind -> unit
val pp_scvmvrewriteparam : Format.formatter -> scvmvrewriteparam -> unit
val pp_scvprintkind : Format.formatter -> scvprintkind -> unit
val pp_scvverbosity : Format.formatter -> scvverbosity -> unit
val pp_scvcmd       : Format.formatter -> scvcmd -> unit

val scvval_to_scvcmd_loc : scvval located -> scvcmd located
val scvverbosity_to_glob : scvverbosity -> int * bool
