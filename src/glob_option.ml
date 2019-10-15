(* Copyright 2019 NXP *)

let v_silent = 0
let v_normal = 1         (* normal printing *)
let v_normal_debug = 2   (* normal printing with extra info i.e full *)
let v_full = 3           (* full printing *)

let full = ref false
let set_full b = full := b

let verbose = ref v_silent

let set_verbose lvl =
  verbose := lvl;
  set_full (v_normal_debug <= lvl)

let if_fprintf lvl =
  if lvl <= !verbose then Format.fprintf
  else Format.ifprintf

let if_printf lvl = if_fprintf lvl Format.std_formatter

let if_eprintf lvl = if_fprintf lvl Format.err_formatter

let print_silent  x = if_printf v_silent x
let eprint_silent x = if_eprintf v_silent x
let print_normal  x = if_printf v_normal x
let eprint_normal x = if_eprintf v_normal x
let print_full    x = if_printf v_full x
let eprint_full   x = if_eprintf v_full x
