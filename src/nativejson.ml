open Typespec

module JsonSerializer : Serializer with type st = Yojson.Basic.json = struct
  type st = Yojson.Basic.json

  let of_float x = `Float x
  let of_int x = `Int x
  let of_bool x = `Int (if x then 1 else 0)
  let of_string x = `String x

  let of_list f x = `List (List.map f x)
  let of_array f x = x |> Array.to_list |> of_list f

  let of_tuple cser x =
    `List (CompositeSerializer.apply cser x)

  let of_record fieldnames cser x =
    `Assoc (List.combine fieldnames (CompositeSerializer.apply cser x))
end;;

module Serializer = Typespec.MakeSerializer(JsonSerializer)

let serialize = Serializer.serialize

