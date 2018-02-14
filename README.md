# Typespec

Compile-time type-safe run-time type library for OCaml.

Use cases:
 * Serialization
 * Deserialization
 * Defaults

Typespec works with records, tuples and arbitrary (custom) types using easy-to-write converters.

Type specifications are fully composable and are designed to be easy to
understand. 

## Usage

Typespec requires the writing of a type specification for any types we wish to work with. 

### A simple example


Let us consider a simple record type:


```ocaml
type phone_number = {
  numtype: string;
  number: string;
}
```

for which we write a run-time type specification:

```ocaml
open Typespec

let phone_number = 
  let get {numtype; number} = (numtype, (number, ())) in
  let set (numtype, (number, ())) = {numtype; number} in
  record {
    converter = {get; set};
    composite = string @> string @> endf;
    fieldnames = ["numtype"; "number"]
  }
```

We can now serialize and deserialize this type in a native Ocaml program:

```ocaml
let phone_serializer = Nativejson.serialize phone_number

let () = 
  {numtype = "mobile"; number = "00000-000000"} |> 
  phone_serializer |> 
  Yojson.Basic.pretty_to_string |> 
  print_endline

```

Output:
```json
{ "numtype": "mobile", "number": "00000-000000" }
```

## Motivation

This library came about on a personal project that saw the benefits of OCaml for systems programming and bucklescript for frontend programming, but resulted in a large amount of time being spent in writing serializers and de-serializers.

In a dynamic language we could use introspection or reflection to obtain information about types at run-time. OCaml does not have such tools.


## How it works

There is no magic in typespec. Behind the scenes there are only GADTs and
suitable recursive data structures. 

## State

Typespec is in the early stages of development. It is usable for
serialization and deserialization. The interface may change and additional
types may be added.

## Future plans

* Support variants
* Support optional types
