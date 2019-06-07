module B = Bigint

type wsize = 
  | U8 
  | U16 
  | U32 
  | U64

type bty = 
  | Bool
  | Int
  | W of wsize 
 
type ty = 
  | Tbase of bty
  | Tarr  of bty * B.zint * B.zint
  | Tmem   

type sign =
  | Signed 
  | Unsigned

type cast = 
  | CW of wsize
  | Cint of sign 

let tint = Tbase Int
let tbool = Tbase Bool
let tw s  = Tbase (W s)

let w8  = tw U8
let w16 = tw U16
let w32 = tw U32
let w64 = tw U64

let ws_string = function
  | U8  -> "w8"
  | U16 -> "w16"
  | U32 -> "w32"
  | U64 -> "w64"

let pp_wsize fmt ws = 
  Format.fprintf fmt "%s" (ws_string ws) 

let pp_bty fmt = function
  | Bool -> Format.fprintf fmt "bool"
  | Int  -> Format.fprintf fmt "int"
  | W s  -> pp_wsize fmt s

let pp_ty fmt = function
  | Tbase bty -> pp_bty fmt bty 
  | Tarr (bty,i1,i2) ->
    Format.fprintf fmt "%a[%a:%a]" pp_bty bty B.pp_print i1 B.pp_print i2
  | Tmem -> Format.fprintf fmt "[]"

let ws_le ws1 ws2 = 
  match ws1, ws2 with
  | U8 , (U8 | U16 | U32 | U64)  -> true
  | U16, U8 -> false
  | U16, (U16 | U32 | U64) -> true
  | U32, (U8 | U16) -> false 
  | U32, (U32 | U64) -> true
  | U64, (U8 | U16 | U32) -> false 
  | U64, U64 -> true

let ty_eq (ty1:ty) ty2 = ty1 = ty2




