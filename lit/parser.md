---
title: Chunk Parser for extracting source code
---

We need to parse a literate Markdown web for source code. The tricky
bit is that code chunks could be directed to different output
files. Furthermore, we might have "names" for each code chunk.

Our main goal is to have a function which will parse a Markdown web,
and produce a collection of lists of code chunks such that each list
of code chunks (a) will be exported to the same file and (b) are
ordered as found in the input.

For debugging purposes, we will also include a function to take a
string, find all the chunks which have a language name (and are not
"examples"), then strip their metadata, and concatenate their
contents.

This means the signatuer for a Tangle-MD parser would look like:

```sml {file=parser.sig}
signature PARSER = sig
  (* collate chunks for debugging purposes *)
  val collate_chunks : Chunk.t list -> Chunk.t list list;
  val export_src_chunks : string -> string;
  val parse : string -> Chunk.t list list;
end;
```

Remember, although this doesn't look like much, that's a good thing:
there are fewer things to manage. Smaller is better when it comes to
programming: there's far less complexity to manage.

# Parser Module

We should begin a module with some documentation for its purposes, as
well as note the license, etc.

```sml {file=parser.sml}
(**
Tangle extracts source code from literate markdown.

Right now, this will look for all the codefenced blocks,
remove the ones without a language, remove the ones which are
considered "example" blocks (which end the language name in an
asterisk), then concatenate them all together in order.

WEB-like named code chunks are not supported.

@author Alex Nelson
@date 2024 August 24
@license MIT
*)

structure Parser :> PARSER = struct
```

When we get started, the first thing we need (besides declaring the
module) is a helper function to locate the next "code fence" in the
Markdown **after** a given position. 
A code fence is three backticks ` ``` ` starting a line
(i.e., either on line 1 or immediately after a "\n" character).

We want to avoid considering **any** character _before_ the given
position. To do this, we can take the substring starting _at_ the
given position, then find the first code fence after it.

Care must be taken to make sure we allow identifying codefences which
begin the Markdown file.

```sml {file=parser.sml}
(*
next_code_fence : string -> int -> int option

Returns the **start** of the next code fence, if any are to be
found.
*)
fun next_code_fence s pos =
  let
    val sub = Substring.extract(s,pos,NONE)
    val len = Substring.size sub;
    fun is_code_fence (i,c) =
      (if 0 = i then 0 = pos
       else #"\n" = Substring.sub(sub,i-1)) andalso
      #"`" = c andalso
      (len > 2 + i) andalso
      (#"`" = Substring.sub(sub, i + 1)) andalso
      (#"`" = Substring.sub(sub, i + 2));
  in
    case (CharVectorSlice.findi is_code_fence
                                (Substring.extract(s,pos,NONE))) of
        NONE => NONE
      | SOME(i,_) => SOME(i + pos)
  end;
```

Now, we want to take everything between two code fences. This would be
everything **after** the initial three backticks (but not including
the leading three backticks) and everything **before** the terminating
three backticks (but not including the terminating three backticks).

So we would want to basically "throw away" the leading and terminating
three backticks.

```sml
(*
harvest_code_block : string -> int -> (substring * int) option

Given a literate markdown content string, and a given starting
position, find the next code block which occurs. If none are
left, then `NONE` is returned.

But if one is found, then its contents (i.e., everything
BETWEEN the three backticks, but NOT INCLUDING the backtick
delimiters) are returned as a `substring option` as well as
the position immediately after the closing three backticks.
 *)
fun harvest_code_block s pos =
    case (next_code_fence s pos) of
        NONE => NONE
      | SOME i =>
        (case (next_code_fence s (i + 3)) of
             NONE => NONE
           | SOME j =>
             SOME ((Substring.substring(s,i+3,j-i-3)),
                   j+3));
```

We can then iteratively look through a given Markdown file, extracting
the contents of all possible code chunks.

This would give us a list of substrings, each substring being the
contents (filler) of a three-backtick sandwich.

```sml
(*
all_code_blocks : string -> substring list

Produce a list of substrings, each substring being the
contents of each code block, i.e., everything after the
initial `` ``` `` and before the terminal code fence ---
INCLUDING the metadata [language, etc.].
 *)
fun all_code_blocks (s : string) =
  let
    fun gather_iter (pos : int) (acc : substring list) =
      case (harvest_code_block s pos) of
          NONE => acc
        | SOME(code,i) => gather_iter i (code::acc);
  in
    gather_iter 0 []
  end;
```

We can then take each possible substring, extract its Metadata, and
form a Chunk object. This gives us a direct and obvious way to
`get_chunks`. 

```sml
(* get_chunks : string -> Chunk.t list

Extract the chunks of code present in a string representing a
Markdown file. This collects all codefenced blocks, extracts
the metacode to accompany each Chunk object. *)
val get_chunks =
  rev o (map ((Chunk.from) o (Metadata.from_codefence_block))) o
  all_code_blocks;
```

A helper function, `export_src_chunks`, which we need for testing
purposes will extract the code for each "non-example" code chunk. 

```sml
(* export_src_chunks : string -> string

Given a string representation of a literate Markdown web file,
extract the Chunks with a language (and which are not
examples), then flatten them into a string for
export.
 *)

val export_src_chunks =
  Substring.concat o (map Chunk.code) o
  (List.filter (fn chunk =>
                   Chunk.has_language chunk andalso
                   not (Chunk.is_example chunk))) o
  get_chunks;
```

For debugging purposes, we also want to extract **all** chunks,
whether or not they're examples.

```sml
(* export_all_chunks : string -> string

Given a string representation of a literate Markdown web file,
extract the Chunks --- with our without a language, which are
or are not examples --- and then flatten them into a string
for export.
 *)
val export_all_chunks =
  Substring.concat o (map Chunk.code) o
  get_chunks;
```

# Collating chunks by name and file

We want to organize code chunks not only by output file (which is
important) but also by name. Although we have not yet parsed the code
chunk for its "name" (Noweb does this in double angled brackets
`<<Example code chunk name>>`, CWEB and WEB use `@<Example code chunk name@>=`),
it is something we will want to do later on.

The idea behind collation is to have an accumulating list of
collections of chunks (each with the same "name" and "file" metadata
attributes). When we have chunks to collate, either they're empty (in
which case...there's nothing to collate, we're done) or not.

When we have a nonempty list of chunks to collate, we start by taking
the initial segment sharing the same name and output file. We then
look through the chunks we have already collated to see if there is a
segment sharing the same name and output file --- if so, then we
append to the end of those chunks the new batch of chunks. Otherwise,
we keep searching.

If there are none (after searching through all the
already-collated-chunks), then we add the new batch to the list of
collated chunks.

```sml
(* collate_chunks : Chunk.t list -> Chunk.t list list

Produces a collection of a list of chunks in the same file,
presented in the same order as given. *)
local
  (* Take the initial sublist of items which share the same name *)
  fun take_with_name chunk acc [] = (rev acc, [])
    | take_with_name chunk acc (c::cs) =
      if Chunk.same_language chunk c andalso
         (Chunk.has_no_name c orelse Chunk.same_name chunk c)
      then take_with_name chunk (c::acc) cs
      else (rev acc, cs);
  (* Append initial segment to same entry of accumulator with
     same name *)
  fun iter acc [] = acc
    | iter acc (chunks as chunk::chunks_tail) =
      let
        val (cs,rest,_) =
          foldl (fn (c,(acc',rest,in_file)) =>
                    if Chunk.same_language chunk c andalso
                       ((in_file andalso
                         Chunk.has_no_name c) orelse
                        (Chunk.same_name chunk c))
                    then (c::acc',rest,true)
                    else (acc', c::rest,false))
                ([],[],false)
                chunks;
      in
        iter ((rev cs)::acc) (rev rest)
      end;
in
fun collate_chunks (chunks : Chunk.t list) =
  iter [] chunks
end;
```

We can collate all the "source" chunks (i.e., the non-example chunks
which need to be exported) by composing what we've already written.

```sml
val collate_src_chunks =
  collate_chunks o (List.filter (not o Chunk.is_example));
```

Last of all, parsing a string for a list of collated-chunks.

```sml
(* parse : string -> Chunk.t list list

Given a string representation for a literate Markdown web,
extract the Code chunks and collate them.
 *)
fun parse (s : string) =
  (collate_src_chunks o get_chunks) s;

end;
```

I should also think about handling chunk inclusions, when/if we end up
supporting "named chunks" like WEB and NOWEB. We'll need to flatten
them at some point. It'll probably be useful to have an actual
associative array data structure before trying to address that.
