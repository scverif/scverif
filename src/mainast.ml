module L = Location

type read_kind =
  | Asm
  | Il

type command = 
  | Read of read_kind * string L.located
  | Exit 

