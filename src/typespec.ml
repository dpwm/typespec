module Converter = struct
  type ('a, 'b) t = {
    get: 'a -> 'b;
    set: 'b -> 'a;
  };;
end;;

module Record' = struct
  type ('a, 'b, 'composite) t = {
    composite : 'composite;
    converter : ('a, 'b) Converter.t;
    fieldnames : string list;
  }
end;;

module Tuple' = struct
  type ('a, 'b, 'composite) t = {
    composite : 'composite;
    converter : ('a, 'b) Converter.t;
  }
end;;

(* TODO: add a custom type *)


type 'a t =
  | Unit  : unit t
  | Int   : int t
  | Float : float t
  | String : string t
  | Bool  : bool t

  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t

  | Tuple : ('a, 'b, 'b composite) Tuple'.t -> 'a t
  | Record : ('a, 'b, 'b composite) Record'.t -> 'a t


and 'a composite =
  | Field: ('b t * 'c composite) -> ('b t * 'c) composite
  | End : unit composite


(* We need an existential type to allow a callback to work. *)
type ts = Typespec : 'a t -> ts;;
let ts : type a. a t -> ts = fun x -> Typespec x;;

(* Handle the case where we wish to map a composite type to a list *)
let rec composite_map : type a b. (ts -> 'd) -> a composite -> 'd list =
  fun f -> function
    | Field (x, xs) -> (x |> ts |> f) :: composite_map f xs
    | End -> []
;;

module Tuple = struct
  include Tuple'
end;;

module Record = struct
  include Record'
end;;

let unit = Unit;;
let int = Int;;
let float = Float;;
let string = String;;
let bool = Bool;;

let list t = List t;;
let array t = Array t;;

(* Just focus on the common case with the tuples. If anybody wants really
 * long tuples we're not stopping them writing converters, just not
 * providing helpers.
 *)

let field t nxt = Field(t, nxt)
let endf = End

let tuple2 t1 t2 =
  let get (x1, x2) = (x1, (x2, ())) in
  let set (x1, (x2, ())) = (x1, x2) in
  Tuple {
    Tuple.converter = {Converter.get; set};
    composite = field t1 @@ field t2 @@ End
    };;

let tuple3 t1 t2 t3 =
  let get (x1, x2, x3) = (x1, (x2, (x3, ()))) in
  let set (x1, (x2, (x3, ()))) = (x1, x2, x3) in
  Tuple {
    Tuple.converter = {Converter.get; set};
    composite = field t1 @@ field t2 @@ field t3 @@ End
  };;

let record composite converter fieldnames = Record {
  Record.fieldnames; composite; converter}

(* We will need a callback to be passed into the serializer. This will
 * effectively be a way to call the parent function that calls it. This allows
 * for really nice things to be done.*)

type 'st serializer_callback = SerializerCallback : ('a t -> 'a -> 'st) -> 'st serializer_callback;;
let serializer_callback x = SerializerCallback x

module type Serializer = sig
  type st 
  val of_unit : unit -> st
  val of_int : int -> st
  val of_float : float -> st
  val of_bool : bool -> st
  val of_string : string -> st
  val of_tuple : st serializer_callback -> ('b, 'c, 'd) Tuple.t -> 'b -> st
end;;


module JsJsonSerializer : Serializer = struct
  open Js.Json

  type st = Js.Json.t
  let of_unit () = number 0.
  let of_float = number
  let of_int x = x |> float_of_int |> number
  let of_bool x = (if x then 1 else 0) |> of_int
  let of_string = Js.Json.string
  let of_tuple f {Tuple.converter; composite} x =
    failwith "foo"
end;;
