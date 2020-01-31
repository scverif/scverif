(* Copyright 2020 - NXP *)
open Common
module MV = Maskverif
module MVP = Maskverif.Prog
module MVE = Maskverif.Expr
module MVU = Maskverif.Util

let mk_op2ttt t s = MVE.Op.make s (Some ([t;t], t)) MVE.NotBij MVE.Other
let mk_op2tttbij t s = MVE.Op.make s (Some ([t;t], t)) MVE.Bij MVE.Other

let o_castsintint = MVE.Op.make "(sint)"    (Some ([MVE.INT], MVE.INT)) MVE.NotBij MVE.Other
let o_castsintb   = MVE.Op.make "(sint)b"   (Some ([MVE.W1],  MVE.INT)) MVE.NotBij MVE.Other
let o_castsintw8  = MVE.Op.make "(sint)w8"  (Some ([MVE.W8],  MVE.INT)) MVE.NotBij MVE.Other
let o_castsintw16 = MVE.Op.make "(sint)w16" (Some ([MVE.W16], MVE.INT)) MVE.NotBij MVE.Other
let o_castsintw32 = MVE.Op.make "(sint)w32" (Some ([MVE.W32], MVE.INT)) MVE.NotBij MVE.Other
let o_castsintw64 = MVE.Op.make "(sint)w64" (Some ([MVE.W64], MVE.INT)) MVE.NotBij MVE.Other

let o_castuintint = MVE.Op.make "(uint)"    (Some ([MVE.INT], MVE.INT)) MVE.NotBij MVE.Other
let o_castuintb   = MVE.Op.make "(uint)b"   (Some ([MVE.W1],  MVE.INT)) MVE.NotBij MVE.Other
let o_castuintw8  = MVE.Op.make "(uint)w8"  (Some ([MVE.W8],  MVE.INT)) MVE.NotBij MVE.Other
let o_castuintw16 = MVE.Op.make "(uint)w16" (Some ([MVE.W16], MVE.INT)) MVE.NotBij MVE.Other
let o_castuintw32 = MVE.Op.make "(uint)w32" (Some ([MVE.W32], MVE.INT)) MVE.NotBij MVE.Other
let o_castuintw64 = MVE.Op.make "(uint)w64" (Some ([MVE.W64], MVE.INT)) MVE.NotBij MVE.Other

let o_ltsint = MVE.Op.make "<s"    (Some ([MVE.INT], MVE.W1)) MVE.NotBij MVE.Other
let o_ltsw8  = MVE.Op.make "<sw8"  (Some ([MVE.W8],  MVE.W1)) MVE.NotBij MVE.Other
let o_ltsw16 = MVE.Op.make "<sw16" (Some ([MVE.W16], MVE.W1)) MVE.NotBij MVE.Other
let o_ltsw32 = MVE.Op.make "<sw32" (Some ([MVE.W32], MVE.W1)) MVE.NotBij MVE.Other
let o_ltsw64 = MVE.Op.make "<sw64" (Some ([MVE.W64], MVE.W1)) MVE.NotBij MVE.Other

let o_ltuint = MVE.Op.make "<u"    (Some ([MVE.INT], MVE.W1)) MVE.NotBij MVE.Other
let o_ltuw8  = MVE.Op.make "<uw8"  (Some ([MVE.W8],  MVE.W1)) MVE.NotBij MVE.Other
let o_ltuw16 = MVE.Op.make "<uw16" (Some ([MVE.W16], MVE.W1)) MVE.NotBij MVE.Other
let o_ltuw32 = MVE.Op.make "<uw32" (Some ([MVE.W32], MVE.W1)) MVE.NotBij MVE.Other
let o_ltuw64 = MVE.Op.make "<uw64" (Some ([MVE.W64], MVE.W1)) MVE.NotBij MVE.Other

let o_lesint = MVE.Op.make "<=s"    (Some ([MVE.INT], MVE.W1)) MVE.NotBij MVE.Other
let o_lesw8  = MVE.Op.make "<=sw8"  (Some ([MVE.W8],  MVE.W1)) MVE.NotBij MVE.Other
let o_lesw16 = MVE.Op.make "<=sw16" (Some ([MVE.W16], MVE.W1)) MVE.NotBij MVE.Other
let o_lesw32 = MVE.Op.make "<=sw32" (Some ([MVE.W32], MVE.W1)) MVE.NotBij MVE.Other
let o_lesw64 = MVE.Op.make "<=sw64" (Some ([MVE.W64], MVE.W1)) MVE.NotBij MVE.Other

let o_leuint = MVE.Op.make "<=u"    (Some ([MVE.INT], MVE.W1)) MVE.NotBij MVE.Other
let o_leuw8  = MVE.Op.make "<=uw8"  (Some ([MVE.W8],  MVE.W1)) MVE.NotBij MVE.Other
let o_leuw16 = MVE.Op.make "<=uw16" (Some ([MVE.W16], MVE.W1)) MVE.NotBij MVE.Other
let o_leuw32 = MVE.Op.make "<=uw32" (Some ([MVE.W32], MVE.W1)) MVE.NotBij MVE.Other
let o_leuw64 = MVE.Op.make "<=uw64" (Some ([MVE.W64], MVE.W1)) MVE.NotBij MVE.Other

let o_lslw8  = MVE.Op.make "<<w8"  (Some ([MVE.W8],  MVE.W8)) MVE.NotBij MVE.Other
let o_lslw16 = MVE.Op.make "<<w16" (Some ([MVE.W16], MVE.W16)) MVE.NotBij MVE.Other
let o_lslw32 = MVE.Op.make "<<w32" (Some ([MVE.W32], MVE.W32)) MVE.NotBij MVE.Other
let o_lslw64 = MVE.Op.make "<<w64" (Some ([MVE.W64], MVE.W64)) MVE.NotBij MVE.Other

let o_lsrw8  = MVE.Op.make ">>w8"  (Some ([MVE.W8],  MVE.W8)) MVE.NotBij MVE.Other
let o_lsrw16 = MVE.Op.make ">>w16" (Some ([MVE.W16], MVE.W16)) MVE.NotBij MVE.Other
let o_lsrw32 = MVE.Op.make ">>w32" (Some ([MVE.W32], MVE.W32)) MVE.NotBij MVE.Other
let o_lsrw64 = MVE.Op.make ">>w64" (Some ([MVE.W64], MVE.W64)) MVE.NotBij MVE.Other

let o_asrw8  = MVE.Op.make ">>sw8"  (Some ([MVE.W8],  MVE.W8)) MVE.NotBij MVE.Other
let o_asrw16 = MVE.Op.make ">>sw16" (Some ([MVE.W16], MVE.W16)) MVE.NotBij MVE.Other
let o_asrw32 = MVE.Op.make ">>sw32" (Some ([MVE.W32], MVE.W32)) MVE.NotBij MVE.Other
let o_asrw64 = MVE.Op.make ">>sw64" (Some ([MVE.W64], MVE.W64)) MVE.NotBij MVE.Other

let o_castww8  = MVE.Op.make "(w8)"  (Some ([MVE.INT], MVE.W8)) MVE.NotBij MVE.Other
let o_castww16 = MVE.Op.make "(w16)" (Some ([MVE.INT], MVE.W16)) MVE.NotBij MVE.Other
let o_castww32 = MVE.Op.make "(w32)" (Some ([MVE.INT], MVE.W32)) MVE.NotBij MVE.Other
let o_castww64 = MVE.Op.make "(w64)" (Some ([MVE.INT], MVE.W64)) MVE.NotBij MVE.Other

let o_aoppint = MVE.Op.make "--"    (Some ([MVE.INT], MVE.INT)) MVE.NotBij MVE.Other
let o_aoppw8  = MVE.Op.make "--w8"  (Some ([MVE.W8],  MVE.W8)) MVE.NotBij MVE.Other
let o_aoppw16 = MVE.Op.make "--w16" (Some ([MVE.W16], MVE.W16)) MVE.NotBij MVE.Other
let o_aoppw32 = MVE.Op.make "--w32" (Some ([MVE.W32], MVE.W32)) MVE.NotBij MVE.Other
let o_aoppw64 = MVE.Op.make "--w64" (Some ([MVE.W64], MVE.W64)) MVE.NotBij MVE.Other

let o_eqint = MVE.Op.make "==int" (Some ([MVE.INT;MVE.INT], MVE.W1)) MVE.NotBij MVE.Other
let o_eqb   = MVE.Op.make "=="    (Some ([MVE.W1;MVE.W1],   MVE.W1)) MVE.NotBij MVE.Other
let o_eqw8  = MVE.Op.make "==w8"  (Some ([MVE.W8;MVE.W8],   MVE.W1)) MVE.NotBij MVE.Other
let o_eqw16 = MVE.Op.make "==w16" (Some ([MVE.W16;MVE.W16], MVE.W1)) MVE.NotBij MVE.Other
let o_eqw32 = MVE.Op.make "==w32" (Some ([MVE.W32;MVE.W32], MVE.W1)) MVE.NotBij MVE.Other
let o_eqw64 = MVE.Op.make "==w64" (Some ([MVE.W64;MVE.W64], MVE.W1)) MVE.NotBij MVE.Other

let o_amulint = mk_op2ttt MVE.INT "*"
let o_amulw8  = mk_op2ttt MVE.w8  "*w8"
let o_amulw16 = mk_op2ttt MVE.w16 "*w16"
let o_amulw32 = mk_op2ttt MVE.w32 "*w32"
let o_amulw64 = mk_op2ttt MVE.w64 "*w64"

let o_aaddint = mk_op2tttbij MVE.INT  "+"
let o_aaddw8  = mk_op2tttbij MVE.w8  "+w8"
let o_aaddw16 = mk_op2tttbij MVE.w16 "+w16"
let o_aaddw32 = mk_op2tttbij MVE.w32 "+w32"
let o_aaddw64 = mk_op2tttbij MVE.w64 "+w64"

let o_asubint = mk_op2tttbij MVE.INT  "-"
let o_asubw8  = mk_op2tttbij MVE.w8  "-w8"
let o_asubw16 = mk_op2tttbij MVE.w16 "-w16"
let o_asubw32 = mk_op2tttbij MVE.w32 "-w32"
let o_asubw64 = mk_op2tttbij MVE.w64 "-w64"


let mk_o oN o8 o16 o32 o64 = function
  | None     -> oN
  | Some U8  -> o8
  | Some U16 -> o16
  | Some U32 -> o32
  | Some U64 -> o64

let o_xor = mk_o 
  MVE.o_addb 
  MVE.o_addw8
  MVE.o_addw16
  MVE.o_addw32
  MVE.o_addw64

let o_and = mk_o
  MVE.o_mulb 
  MVE.o_mulw8 
  MVE.o_mulw16 
  MVE.o_mulw32 
  MVE.o_mulw64 

let o_not = mk_o
  MVE.o_negb
  MVE.o_negw8 
  MVE.o_negw16
  MVE.o_negw32
  MVE.o_negw64

let o_add = mk_o
  o_aaddint
  o_aaddw8
  o_aaddw16
  o_aaddw32
  o_aaddw64

let o_sub = mk_o 
  o_asubint
  o_asubw8
  o_asubw16
  o_asubw32
  o_asubw64

let o_mul = mk_o 
  o_amulint
  o_amulw8
  o_amulw16
  o_amulw32
  o_amulw64

let o_opp = mk_o
  o_aoppint
  o_aoppw8
  o_aoppw16
  o_aoppw32
  o_aoppw64

let mk_ob oi ob o8 o16 o32 o64 = function
  | Int   -> oi
  | Bool  -> ob
  | W U8  -> o8
  | W U16 -> o16
  | W U32 -> o32
  | W U64 -> o64


let o_castsint = mk_ob 
  o_castsintint
  o_castsintb
  o_castsintw8
  o_castsintw16
  o_castsintw32
  o_castsintw64

let o_castuint = mk_ob
  o_castuintint
  o_castuintb
  o_castuintw8
  o_castuintw16
  o_castuintw32
  o_castuintw64

let o_lts = mk_o 
  o_ltsint
  o_ltsw8
  o_ltsw16
  o_ltsw32
  o_ltsw64

let o_ltu = mk_o
  o_ltuint
  o_ltuw8
  o_ltuw16
  o_ltuw32
  o_ltuw64

let o_les = mk_o
  o_lesint
  o_lesw8
  o_lesw16
  o_lesw32
  o_lesw64

let o_leu = mk_o 
  o_leuint
  o_leuw8
  o_leuw16
  o_leuw32
  o_leuw64

let mk_ow o8 o16 o32 o64 = function
  | U8  -> o8
  | U16 -> o16
  | U32 -> o32
  | U64 -> o64

let o_lsl = mk_ow 
  o_lslw8
  o_lslw16
  o_lslw32
  o_lslw64

let o_lsr = mk_ow
  o_lsrw8
  o_lsrw16
  o_lsrw32
  o_lsrw64

let o_asr = mk_ow
  o_asrw8
  o_asrw16
  o_asrw32
  o_asrw64

let o_cast_w = mk_ow
  o_castww8
  o_castww16
  o_castww32
  o_castww64

let o_eq = mk_ob
  o_eqb
  o_eqint
  o_eqw8
  o_eqw16
  o_eqw32
  o_eqw64
