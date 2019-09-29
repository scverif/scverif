open Protocol_conv
open Runtime
open Utils
open Common

type scvstring = [%import: Scv.scvstring]

let pp_scvstring fmt s = Format.fprintf fmt "%a" pp_string s

type scvval = [%import: Scv.scvval]

let rec pp_scvval fmt v =
  match v with
  | SCVNull -> Format.fprintf fmt "null"
  | SCVBool b -> Format.fprintf fmt "%b" b
  | SCVInt i -> Format.fprintf fmt "%a" B.pp_print i
  | SCVString s -> Format.fprintf fmt "%s" s
  | SCVMap m ->
    let pp_map fmt (k,v) =
      match v with
      | SCVMap _ ->
        Format.fprintf fmt "@[<v>%a:@ @[<v>  %a@]@]"
          pp_scvstring k
          pp_scvval v
      | _ ->
        Format.fprintf fmt "@[<v>%a: %a@]"
          pp_scvstring k
          pp_scvval v in
    Format.fprintf fmt "@[<v>%a@];"
      (pp_list "@ " pp_map) m
  | SCVList l ->
    Format.fprintf fmt "list [%a]"
      (pp_list ", " (pp_scvval)) l

module Driver : Ppx_protocol_driver.Driver with type t = scvval = struct

  type t = scvval

  type error = string * scvval option
  exception Protocol_error of error

  let make_error ?value msg = (msg, value)

  let to_string_hum t = Format.asprintf "%a" pp_scvval t

  let error_to_string_hum: error -> string = function
    | (s, Some t) -> Printf.sprintf "%s. T: '%s'" s (to_string_hum t)
    | (s, None) -> s

  (* Register exception printer *)
  let () = Caml.Printexc.register_printer (function
    | Protocol_error err -> Some (error_to_string_hum err)
    | _ -> None)

  let raise_errorf t fmt =
    Caml.Printf.kprintf (fun s -> raise (Protocol_error (s, Some t))) fmt

  let raise_errorn fmt =
    Caml.Printf.kprintf (fun s -> raise (Protocol_error (s, None))) fmt

  let try_with: (scvval -> 'a) -> scvval -> ('a, error) Runtime.result = fun f t ->
    match f t with
    | v -> Ok v
    | exception (Protocol_error e) -> Error e

  let of_list l = SCVList l
  let to_list = function SCVList l -> l | _ -> failwith "SCVList expected but not encountered"
  let is_list = function SCVList _ -> true | _ -> false

  let of_alist a = SCVMap a
  let to_alist t =
    match t with
    | SCVMap al -> al
    | _ -> failwith "SCVMap expected but not encountered"

  let is_alist = function SCVMap _ -> true | _ -> false


  let of_int i = SCVInt (B.of_int i)
  let to_int = function SCVInt i -> B.to_int i | e -> raise_errorf e "SCVInt expected but not found"

  let of_int32 i = SCVInt (B.of_int (Int32.to_int i))
  let to_int32 t =
    match t with
    | SCVInt i -> B.to_int i |> Int32.of_int
    | e -> raise_errorf e "SCVInt expected but not encountered"

  let of_int64 i = SCVInt (B.of_int (Int64.to_int i))
  let to_int64 t =
    match t with
    | SCVInt i -> B.to_int i |> Int64.of_int
    | e -> raise_errorf e "SCVInt expected but not encountered"

  let of_nativeint i = SCVInt (B.of_int (Nativeint.to_int i))
  let to_nativeint t =
    match t with
    | SCVInt i -> B.to_int i |> Nativeint.of_int
    | e -> raise_errorf e "SCVInt expected but not encountered"

  let of_string s = SCVString s
  let to_string = function SCVString s -> s | e -> raise_errorf e "SCVString expected but type not found"
  let is_string = function SCVString _ -> true | _ -> false

  module Float = BatFloat

  let of_float f =
    let i = Float.to_int f in
    if Float.approx_equal (Float.of_int i) f then
      SCVInt (B.of_int i)
    else
      raise_errorn "Conversion of float to int failed due to precision loss"
  let to_float = function SCVInt i -> B.to_int i |> Float.of_int | e -> raise_errorf e "SCVInt expected"

  let of_char c = SCVString (String.of_char c)
  let to_char t =
    match t with
    | SCVString s ->
      if (BatString.length s) == 1 then
        s.[0]
      else
        raise_errorn "Conversion from SCVString to char failed, got more than one character"
    | e -> raise_errorf e "String type not found"

  let of_bool b = SCVBool b
  let to_bool = function SCVBool b -> b | e -> raise_errorf e "SCVBool type not found"

  let of_unit () = raise_errorn "Unit type not supported in SCV value type"
  let to_unit = function e -> raise_errorf e "Conversion to unit type not allowed in SCV"

  let null = SCVNull
  let is_null = function SCVNull -> true | _ -> false
end

include Ppx_protocol_driver.Make(Driver)(Ppx_protocol_driver.Default_parameters)
module Make(P: Ppx_protocol_driver.Parameters) = Ppx_protocol_driver.Make(Driver)(P)

let of_scvval_exn t = t
let of_scvval t = Ok t
let to_scvval t = t
