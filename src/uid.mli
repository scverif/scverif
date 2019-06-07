type t 
val fresh : unit -> t
val compare : t -> t -> int
val equal   : t -> t -> bool
val pp : Format.formatter -> t -> unit
