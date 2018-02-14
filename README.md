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

In a dynamic language we could use introspection or reflection to obtain information about types at run-time. Even if we had those tools, there is still an innate advantage in using a more structure representation. Python has [marshmallow](https://github.com/marshmallow-code/marshmallow) which requires duplication. Typespec can be thought of as a similar tool for Ocaml.

## How it works

There is no magic in typespec. There is no Obj.magic.  Behind the scenes
there are only GADTs and suitable recursive data structures. A mapping from
the type described by the typespec is created when the typespec is passed to
the serializer function.

Internally, every function lookup is done at serializer creation time. This means that there is no lookup during serialization, only when the serializer is created. In this way the closure returned can be thought of as executing a bytecode with function pointers. The type system guarantees that what we pass in is structurally compatible.

## State

Typespec is in the early stages of development. It is usable for
serialization and deserialization. The interface may change and additional
types may be added.

## Future plans

* Support variants
* Support optional types
* Improve performance
