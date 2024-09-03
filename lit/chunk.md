---
title: "Chunk" Abstraction
---

We want to form an abstraction around a chunk of code, which will
track the string of code to be exported **and** the metadata on the
chunk.

## Interface

Like any abstract data type, we have a module named `Chunk` with a
type `t` (so we refer to `Chunk.t` as the type for a Chunk of code).

We need a constructor `new : Metadata.t -> substring -> Chunk` taking
a Metadata object and the source code substring stripped of metadata,
then producing a Chunk object. For convenience, we use `from :
Metadata.t * substring -> Chunk` as another smart constructor, just a
Curried form of the constructor, for usage when composing with the
Metadata parser function.

Then we just need accessor functions to get (but not mutate) various
fields from the `Chunk.t` abstraction: we need the code substring, the
Metadata abstraction, testing if it's an example snippet, and get the
language (if it has one).

```sml {file=chunk.sig}
signature CHUNK = sig
  type t;
  type Chunk = t;

  val new : Metadata.t -> substring -> Chunk;
  val from : Metadata.t * substring -> Chunk
  val metadata : Chunk -> Metadata.t;
  val code : Chunk -> substring;
  val is_example : Chunk -> bool;
  val language : Chunk -> string option;
  val has_language : Chunk -> bool;
  val same_language : Chunk -> Chunk -> bool;
  val same_name : Chunk -> Chunk -> bool;
  val has_no_name : Chunk -> bool;
  val eq : Chunk -> Chunk -> bool;
  val file : Chunk -> string;
end;
```

## Implementation

The implementation amounts to tracking an ordered pair of a
`Metadata.t` object and a substring. Everything else is just an
"obvious" accessor.

```sml {file=chunk.sml}
structure Chunk :> CHUNK = struct
  type t = { meta : Metadata.t
           , code : substring
           };

  type Chunk = t;

  fun new m s = {meta = m, code = s};

  fun from (m,s) = {meta = m, code = s};

  fun metadata ({meta,...} : t) = meta;

  fun code ({code,...} : t) = code;

  fun is_example ({meta, ...} : t) = Metadata.is_example meta;

  fun language ({meta, ...} : t) = Metadata.language meta;

  fun has_language chunk =
    NONE <> (language chunk);
```

We will want to also test if two chunks _both_ have languages _and_
they are the same languages. If both chunks **lack** a "language"
attribute, this should return false.

```sml
  fun same_language lhs rhs =
    has_language lhs andalso
    has_language rhs andalso (* provably symmetric *)
    language lhs = language rhs;
```

Testing if two chunks have the same name (or if a chunk lacks a name)
amounts to delegating the work to the Metadata.

Testing for equality of chunks amounts to checking if the Metadata for
the chunks are equal, and also if the chunks refer to the same
substring (in the sense of "pointing" to the same fragment of the same
string). 

```sml
  fun same_name lhs rhs =
    Metadata.same_name (metadata lhs) (metadata rhs);

  val has_no_name =
    Metadata.has_no_name o metadata;

  fun eq lhs rhs =
    (Metadata.eq (metadata lhs) (metadata rhs)) andalso
    ((Substring.base (code lhs)) = (Substring.base (code rhs)));
```

The last function worth discussing is an accessor function to obtain
the file name for the code chunk. If it is missing, then return the
empty string.

This just simplifies life when debugging situations.

```sml
  fun file this =
    case (Metadata.get (metadata this) "file") of
        NONE => ""
      | SOME f => f;
end;
```
