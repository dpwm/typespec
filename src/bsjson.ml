open Typespec

module JsJsonSerializer : Serializer with type st = Js.Json.t = struct

  type st = Js.Json.t

  open Js.Json

  let of_float = number
  let of_int x = x |> float_of_int |> number
  let of_bool x = (if x then 1 else 0) |> of_int
  let of_string = Js.Json.string

  let of_list f = fun x -> x |> List.map f |> Array.of_list |> Js.Json.array
  let of_array f = fun x -> x |> Array.map f |> Js.Json.array

  let of_tuple cser x = 
    CompositeSerializer.apply cser x |> Array.of_list |> Js.Json.array

  let of_record _ = of_tuple
end;;

let fail_if_none err = function
  | Some x -> x
  | None -> failwith err


module JsJsonDeserializer : Deserializer with type st = Js.Json.t = struct
  type st = Js.Json.t

  open Js.Json

  let to_float x = 
    x |> decodeNumber |> fail_if_none "Expected float"

  let to_int x = 
    x |> to_float |> int_of_float

  let to_string x =
    x |> decodeString |> fail_if_none "Expected string"

  let to_bool x = 
    if to_int x > 0 then true else false

  let to_array f x =
    x |> decodeArray |> fail_if_none "Expected array" |>
    Js.Array.map f

  let to_list f x =
    to_array f x |> Array.to_list

  let to_tuple fxs x =
    x |> decodeArray |> fail_if_none "Expected array" |> Array.to_list |>
    CompositeDeserializer.apply fxs

  let to_record _ = to_tuple
    

end;;

module Serializer = Typespec.MakeSerializer(JsJsonSerializer)
module Deserializer = Typespec.MakeDeserializer(JsJsonDeserializer)

let serialize = Serializer.serialize
let deserialize = Deserializer.deserialize
