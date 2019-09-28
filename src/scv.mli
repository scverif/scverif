open Common

type scvstring = string

type scvval =
  | SCVNull
  | SCVBool of bool
  | SCVInt of B.zint
  | SCVString of scvstring
  | SCVMap of (scvstring * scvval) list
  | SCVList of scvval list

type t = scvval

val pp_scvstring  : Format.formatter -> scvstring -> unit
val pp_scvval     : Format.formatter -> scvval -> unit

include Protocol_conv.Runtime.Driver with type t := scvval

module Make(P: Ppx_protocol_driver.Parameters) : (Protocol_conv.Runtime.Driver with type t = scvval)

val of_scvval_exn : scvval -> scvval
val of_scvval     : scvval -> (scvval, error) Protocol_conv.Runtime.result
val to_scvval     : scvval -> scvval
