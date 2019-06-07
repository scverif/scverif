type t = int

let count = ref (-1)

let fresh () = incr count; !count

let compare i1 i2 = i1 - i2
let equal i1 i2 = i1 == i2
