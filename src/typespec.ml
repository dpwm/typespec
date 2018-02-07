module Converter = struct
  type ('a, 'b) t = {
    get: 'a -> 'b;
    set: 'b -> 'a;
  };;
end;;

module Record = struct
  type ('a, 'b, 'composite) t = {
    composite : 'composite;
    converter : ('a, 'b) Converter.t;
    fieldnames : string list;
  }
end;;

module Tuple = struct
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

  | Tuple : ('a, 'b, 'b composite) Tuple.t -> 'a t
  | Record : ('a, 'b, 'b composite) Record.t -> 'a t


and 'a composite =
  | Field: ('b t * 'c composite) -> ('b * 'c) composite
  | End : unit composite


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

(* A simple example of how to define a record.
type xt = {
  a: int;
  b: int;
  c: int;
}

let xtt = 
  let get {a; b; c} = (a, (b, (c, ()))) in
  let set (a, (b, (c, ()))) = {a; b; c} in
  record (field int @@ field int @@ field int @@ endf) {Converter.get; set} ["a"; "b"; "c"];;
*)
