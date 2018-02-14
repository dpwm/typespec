# Typespec

Compile-time type-safe run-time type library for OCaml.

Use cases:
 * Serialization
 * Deserialization
 * Defaults

Works with records, tuples and arbitrary (custom) types using easy-to-write converters.

## Motivation

This library came about on a personal project that saw the benefits of OCaml for systems programming and bucklescript for frontend programming, but resulted in a large amount of time being spent in writing serializers and de-serializers.

## Usage

Typespec requires the writing of a type specification for any types we wish to work with. 

### A simple example

```ocaml
open Typespec

type phone_number = {
  numtype: string;
  number: string;
}

let phone_number = 
  let get {numtype; number} = (numtype, (number, ())) in
  let set (numtype, (number, ())) = {numtype; number} in
  record {
    converter = {get; set};
    composite = string @> string @> endf;
    fieldnames = ["numtype"; "number"]
  }

```



## How it works

There is no magic in typespec. Behind the scenes there are only GADTs and
suitable recursive data structures.

