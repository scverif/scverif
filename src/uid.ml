type t = int

let count = ref (-1)

let fresh () = incr count; !count
