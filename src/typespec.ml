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

type 'a t =
  | Unit  : unit t
  | Int   : int t
  | Float : float t
  | String : string t
  | Bool  : bool t

  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t

  | Record : ('a, 'b, 'b composite) Record.t -> 'a t
  | Tuple : ('a, 'b, 'b composite) Tuple.t -> 'a t


and 'a composite =
  | Field: ('b t * 'c composite) -> ('b * 'c) composite
  | End : unit composite

