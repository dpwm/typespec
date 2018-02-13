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


module CompositeSerializer = struct
  type ('a, 'st) t = 
    | F: (('a -> 'st) * ('b, 'st) t) -> ('a * 'b, 'st) t
    | E: (unit, 'st) t 

  let rec apply: type a. (a, 'st) t -> a -> 'st list =
    fun s x -> 
    match s with
      | F (f, fxs) -> (f (fst x)) :: apply fxs (snd x)
      | E -> []
end;;

module CompositeDeserializer = struct
  type ('a, 'st) t = 
    | F: (('st -> 'a) * ('b, 'st) t) -> ('a * 'b, 'st) t
    | E: (unit, 'st) t 

  let rec apply: type a. (a, 'st) t -> 'st list -> a =
    fun s xs -> 
    match s with
      | F (f, fxs) -> (List.hd xs |> f, apply fxs (List.tl xs))
      | E -> ()

end;;

module Tuple = struct
  include Tuple'
end;;

module Record = struct
  include Record'
end;;


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


  val of_int : int -> st
  val of_float : float -> st
  val of_bool : bool -> st
  val of_string : string -> st

  val of_list : ('a -> st) -> 'a list -> st
  val of_array : ('a -> st) -> 'a array -> st

  val of_tuple : 
    ('a, st) CompositeSerializer.t -> 'a -> st
  val of_record :
    string list -> ('a, st) CompositeSerializer.t -> 'a -> st
end;;
  
module type Deserializer = sig
  type st 

  val to_int : st -> int
  val to_float : st -> float
  val to_bool : st -> bool
  val to_string : st -> string

  val to_list : (st -> 'a) -> st -> 'a list
  val to_array : (st -> 'a) -> st -> 'a array

  val to_tuple :
    ('a, st) CompositeDeserializer.t -> st -> 'a

  val to_record : 
    string list -> ('a, st) CompositeDeserializer.t ->  st -> 'a
end;;


module MakeSerializer (S : Serializer) = struct
  type st = S.st

  type 'a cst = ('a, st) CompositeSerializer.t

  let rec serialize : type a. a t -> (a -> st) =
    let rec serialize_composite : type a. a composite -> a cst =
      let open CompositeSerializer in
      function
        | Field (x, xs) -> F (serialize x, serialize_composite xs)
        | End -> E
    in
    function 
      | String -> S.of_string
      | Bool -> S.of_bool
      | List t -> t |> serialize |> S.of_list
      | Array t -> t |> serialize |> S.of_array
      | Int -> S.of_int
      | Float -> S.of_float
      | Tuple {composite; converter} -> 
          let f = 
            composite |> 
            serialize_composite |> 
            S.of_tuple in
          fun x -> 
            x |> converter.get |> f 

      | Record {composite; converter; fieldnames} ->
          let f = 
            composite |> 
            serialize_composite |> 
            S.of_record fieldnames
          in
          fun x ->
            x |> converter.get |> f

  ;;
end;;

module MakeDeserializer (S : Deserializer) = struct
  type st = S.st

  type 'a cst = ('a, st) CompositeDeserializer.t

  let rec deserialize : type a. a t -> (st -> a) =
    let rec deserialize_composite : type a. a composite -> a cst =
      let open CompositeDeserializer in
      function
        | Field (x, xs) -> F (deserialize x, deserialize_composite xs)
        | End -> E
    in
    function 
      | String -> S.to_string
      | Bool -> S.to_bool
      | List t -> t |> deserialize |> S.to_list
      | Array t -> t |> deserialize |> S.to_array
      | Int -> S.to_int
      | Float -> S.to_float
      | Tuple {composite; converter} -> 
          let f = 
            composite |> 
            deserialize_composite |> 
            S.to_tuple in
          fun x -> 
            x |> f |> converter.set

      | Record {composite; converter; fieldnames} ->
          let f = 
            composite |> 
            deserialize_composite |> 
            S.to_record fieldnames
          in
          fun x ->
            x |> f |> converter.set 

  ;;
end;;

