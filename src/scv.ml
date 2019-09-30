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
  | SCVList l ->
    Format.fprintf fmt "list[%a]"
      (pp_list ", " (pp_scvval)) l
  | SCVRecord a ->
    let pp_scvrecelem fmt (k,v) =
    Format.fprintf fmt "%a: %a"
      pp_scvstring k
      pp_scvval v in
    Format.fprintf fmt "record(@[<v>%a@])"
      (pp_list "@ " pp_scvrecelem) a
  | SCVMap(i, []) ->
    Format.fprintf fmt "map(@[<v>%a: noargs@])"
      pp_scvstring i
  | SCVMap(i, vs) ->
    Format.fprintf fmt "map(@[<v>%a:@   @[<v>%a@]@])"
      pp_scvstring i
      (pp_list "@ " pp_scvval) vs

(*let rec pp_scvrecord fmt r =
  match r with
  | `SCVRecord(i,v) -> Format.fprintf fmt "%a: %a" pp_scvstring i pp_scvval v
  | `SCVNull -> Format.fprintf fmt "null"

let pp_scv fmt v =
  match v with
  | `SCVRecord _ -> pp_scvrecord fmt v
  | `SCVBool _
  | `SCVInt _
  | `SCVString _
  | `SCVList _
  | `SCVNull
  | `SCVMap _ -> pp_scvval fmt v*)

module Driver : Ppx_protocol_driver.Driver with type t = scvval = struct

  type t = scvval

  type error = string * scvval option
  exception Protocol_error of error

  let make_error ?value msg = (msg, value)

  let to_string_hum (t:scvval) = Format.asprintf "%a" pp_scvval t

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

  let of_list l =
    SCVList l

  let to_list = function
    | SCVList vs -> vs
    | e -> raise_errorf e "List type not found"

  let to_list =
    function
    | SCVList l -> l
    (*    | SCVRecord l -> [SCVString k;v] *)
    | e -> raise_errorf e "SCVList expected but not encountered"

  let is_list = function
    | SCVRecord _
    | SCVList _ -> true
    | _ -> false

  let of_alist a = SCVRecord a
  let to_alist t = hierror "Scv.to_alist" None "of_alist unsupported"

  let is_alist = function
    | SCVRecord _ -> true
    | _ -> false

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

  let of_unit () = SCVNull
  let to_unit = function SCVNull -> () | e -> raise_errorf e "Expected SCVNull for Unit]"

  let null = SCVNull
  let is_null = function SCVNull -> true | _ -> false

  (* ineffective:
     despite part of ppx_protocol_conv.Runtime.Driver these functions are never called

     let of_variant = fun name spec -> (?)
     let to_variant = fun spec -> (?)
  *)
end

module P = Ppx_protocol_driver.Default_parameters

(* This includes the Make module of ppx_protocol_driver (NOT runtime.driver) *)
include Ppx_protocol_driver.Make(Driver)(P)

(* Why do we need this? all the internal functions are defined as toplevel by the include *)
module Make(P: Ppx_protocol_driver.Parameters) = Ppx_protocol_driver.Make(Driver)(P)

(* override the included ppx_protocol_driver *_variant functions *)
let to_tuple: (t, 'constr, 'b) Tuple_in.t -> 'constr -> t -> 'b = fun spec constr->
  let f = Helper.to_tuple spec constr in
  function
  | SCVList ts -> f ts
  | SCVRecord a -> f (List.map (fun (k,v) -> SCVMap(k,[v])) a)
  | t -> hierror "Scv.to_tuple ext" None "List expected to tuple %a" pp_scvval t

let of_tuple: type a. (t, a, t) Tuple_out.t -> a = fun spec ->
  Helper.of_tuple (fun l -> SCVList l) spec

let to_list: (t -> 'a) -> t -> 'a list = fun to_value_fun -> function
  | SCVList vs -> List.map to_value_fun vs
  | e -> hierror "Scv.to_list ext" None "List type not found %a" pp_scvval e

let of_list: ('a -> t) -> 'a list -> t = fun of_value_fun v ->
  SCVList (List.map of_value_fun v)

let to_record: (t, 'constr, 'b) Record_in.t -> 'constr -> t -> 'b = fun spec constr -> function
  | SCVRecord rs -> Helper.to_record spec constr rs
  | t -> hierror "Scv.to_record ext" None "Expected map for record %a" pp_scvval t

let of_record: type a. (t, a, t) Record_out.t -> a = fun spec ->
  Helper.of_record ~omit_default:false (fun rs -> SCVRecord rs) spec

let to_variant: (t, 'a) Variant_in.t list -> t -> 'a = fun spec -> function
  | SCVMap (name, args) -> Helper.to_variant spec name args
  | t -> hierror "Scv.to_variant ext" None "Variant expected %a" pp_scvval t

let of_variant: string -> (t, 'a, t) Tuple_out.t -> 'a = fun name spec ->
  Helper.of_variant (fun name args -> SCVMap (name, args)) name spec

let to_bool = function
  | SCVRecord [(_, SCVBool b)] -> b
  | SCVBool b -> b
  | SCVMap (_, [SCVBool b]) -> b
  | e -> hierror "Scv.to_bool ext" None "Bool type not found %a" pp_scvval e

(*
let to_record: (t, 'constr, 'b) Record_in.t -> 'constr -> t -> 'b = fun spec constr -> function
  | SCVRecord a -> Helper.to_record spec constr a
  | t -> hierror "Scv.to_record" None "Expected SCVRecord for record"

let of_record: type a. (t, a, t) Record_out.t -> a = fun spec ->
  Helper.of_record ~omit_default:false (fun a -> SCVRecord a) spec

let to_tuple: (t, 'constr, 'b) Tuple_in.t -> 'constr -> t -> 'b = fun spec constr->
  function
  | SCVList ts -> Helper.to_tuple spec constr ts
  | SCVRecord rs -> Helper.to_tuple spec constr rs
  | t -> hierror "Scv.to_tuple" None "SCVList expected got %a@." pp_scvval t

let of_tuple: type a. (t, a, t) Tuple_out.t -> a = fun spec ->
  Helper.of_tuple (fun l -> SCVList l) spec

(*let of_variant =
  let serialize (name:string) (args:scvval list) = SCVMap(name, args) in
  fun name spec -> Helper.of_variant serialize name spec*)
(*Helper.of_variant (fun name args -> SCVMap [(name, Driver.)]) name spec*)

let of_variant: string -> (t, 'a, t) Tuple_out.t -> 'a = fun name spec ->
  Helper.of_variant (fun name args ->
      begin
        match args with
        | [] -> SCVMap (name, None)
        | _ -> SCVMap (name, Some (SCVList args))
      end) name spec

let to_variant: (t, 'a) Variant_in.t list -> t -> 'a =
  fun spec -> function
    | SCVString name -> Helper.to_variant spec name [SCVNull]
    | SCVMap(name, Some arg) -> Helper.to_variant spec name [arg]
    | SCVMap(name, None) -> Helper.to_variant spec name [SCVNull]
    | t -> Format.printf "%a@ " pp_scvval t; Utils.hierror "SCV.to_variant" None "SCVMap with single record expected"
*)

(* this has the wrong type of the Make module, thus the changes take no effect
module Make(P: Ppx_protocol_driver.Parameters) = struct
  module P = Ppx_protocol_driver.Default_parameters
  include Ppx_protocol_driver.Make(Driver)(P)

  let of_variant = fun name spec ->
    Helper.of_variant (fun name args -> assert false; SCVMap [(name, SCVBool false)]) name spec

  let to_variant: (t, 'a) Variant_in.t list -> t -> 'a = fun spec -> function
    | t -> assert false (*"SCVMap with single record expected"*)
end
*)

(* not working, of_variant and to_variant have no impact
   why???

  module Make = struct
  module P = Ppx_protocol_driver.Default_parameters
  include Ppx_protocol_driver.Make(Driver)(P)
  module D = Driver

  let of_variant = fun name spec ->
    Helper.of_variant (fun name args -> assert false ; SCVMap [(name, SCVBool false)]) name spec

  let to_variant: (t, 'a) Variant_in.t list -> t -> 'a =
    fun spec -> assert false
  end
*)

let of_scvval_exn t = t
let of_scvval t = Ok t
let to_scvval t = t
