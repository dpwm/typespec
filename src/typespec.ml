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
  | Field: ('a t * 'b composite) -> ('a * 'b) composite
  | End : unit composite


module Ser = struct
  type 'a v = 'a t
  type ('a, 'st) t = 
    | F: (('a -> 'st) * ('b, 'st) t) -> ('a * 'b, 'st) t
    | E: (unit, 'st) t 

  let (@@>>>) a b = F (a, b)
  let e = E
end
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
let (@>) = field
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

module type S = sig
  type 'a t
end;;


module type Serializer = sig
  type st 


  val of_unit : unit -> st
  val of_int : int -> st
  val of_float : float -> st
  val of_bool : bool -> st
  val of_string : string -> st

  val of_list : ('a -> st) -> 'a list -> st
  val of_array : ('a -> st) -> 'a array -> st

  (* This will be the most complicated one, because until now we have only
   * returned a function. Ideally we would return a closure here *)

  val of_tuple : ('a, st) Ser.t -> 'a -> st

end;;





module JsJsonSerializer : Serializer with type st = Js.Json.t = struct

  type st = Js.Json.t

  open Js.Json


  let of_unit () = number 0.
  let of_float = number
  let of_int x = x |> float_of_int |> number
  let of_bool x = (if x then 1 else 0) |> of_int
  let of_string = Js.Json.string

  let of_list f = fun x -> x |> List.map f |> Array.of_list |> Js.Json.array
  let of_array f = fun x -> x |> Array.map f |> Js.Json.array

  let of_tuple _ = 
    failwith "foo"
  ;;
end;;



(* Too many functors *)

type 'a v = 'a t;;

(* Do not worry so much about the functors for now *)

let rec apply_composite : type a. (a, 'st) Ser.t -> a -> 'st list =
  fun s x -> 
  match s with
    | Ser.F (f, fxs) -> (f (fst x)) :: apply_composite fxs (snd x)
    | Ser.E -> []
;;

module MakeSerializer (S : Serializer) = struct
  type st = S.st

  let rec serialize : type a. a t -> (a -> st) =
    function 
      | Unit -> S.of_unit
      | String -> S.of_string
      | Bool -> S.of_bool
      | List t -> t |> serialize |> S.of_list
      | Array t -> t |> serialize |> S.of_array
      | Int -> S.of_int
      | Float -> S.of_float
      | Tuple {composite; converter} -> 
          let scomp = composite |> serialize_composite in
          fun x -> 
            x |> converter.get |> S.of_tuple scomp

      | Record _ -> failwith "not implemented"
  and serialize_composite : type a. a composite -> (a, st) Ser.t =
    function
      | Field (x, xs) -> Ser.F (serialize x, serialize_composite xs)
      | End -> Ser.E
  ;;
end;;

type 'a tree = 
  | Tree : ('a t * 'b tree) -> ('a * 'b) tree
  | TEnd : unit tree

let (@>>>) a b = Tree (a, b)


(* This is great and we can do something with it. *)
let () =



  ()

