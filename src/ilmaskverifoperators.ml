(* Copyright 2020 - NXP *)
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
