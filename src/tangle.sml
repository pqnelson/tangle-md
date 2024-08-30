(**
Tangle extracts source code from literate markdown.

Right now, this will look for all the codefenced blocks,
remove the ones without a language, remove the ones which are
considered "example" blocks (which end the language name in an
asterisk), then concatenate them all together in order.

WEB-like named code chunks are not supported.

2^32 ~ 4 * (10^3)^6

@author Alex Nelson
@date 2024 August 24
@license MIT
*)

(*
next_code_fence : string -> int -> int option

Returns the **start** of the next code fence, if any are to be
found.
*)
fun next_code_fence s pos =
  let
    val len = size s;
    fun is_code_fence (i,c) =
      (i >= pos) andalso
      (0 = i orelse #"\n" = String.sub(s,i-1)) andalso
      #"`" = c andalso
      (len > (pos + 2)) andalso
      (#"`" = String.sub(s, i + 1)) andalso
      (#"`" = String.sub(s, i + 2));
  in
    case (CharVector.findi is_code_fence s) of
        NONE => NONE
      | SOME(i,_) => SOME(i)
  end;

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

(* get_chunks : string -> Chunk.t list

Extract the chunks of code present in a string representing a
Markdown file. This collects all codefenced blocks, extracts
the metacode to accompany each Chunk object. *)
val get_chunks =
  (map ((Chunk.from) o (Metadata.from_codefence_block))) o
  all_code_blocks;

(* form_src : string -> string

For a string representing a Markdown file, produce a string
consisting of all the code in the file.
 *)
val form_src =
  Substring.concat o (map Chunk.code) o
  (List.filter (fn chunk =>
                   Chunk.has_language chunk andalso
                   not (Chunk.is_example chunk))) o
  get_chunks;

(* collate_chunks : Chunk.t list -> Chunk.t list list

Produces a collection of a list of chunks in the same file,
presented in the same order as given. *)
local
  fun name chunk =
    (Metadata.get (Chunk.metadata chunk) "name",
     Metadata.get (Chunk.metadata chunk) "file");
  (* Take the initial sublist of items which share the same name *)
  fun take_with_name n [] acc = (rev acc, [])
    | take_with_name n (chunks as (c::cs)) acc =
      if name c = n orelse name c = (NONE,NONE)
      then take_with_name n cs (c::acc)
      else (rev acc, cs);
  fun iter acc [] = acc
    | iter acc chunks =
      let
        val (cs,rest) = take_with_name (name (hd chunks)) chunks [];
        val acc' = map (fn coll =>
                           if (name (hd coll)) = (name (hd cs))
                           then coll @ cs
                           else coll)
                       acc;
      in
        iter acc' rest
      end;
in
fun collate_chunks (chunks : Chunk.t list) =
  iter [] chunks
end;

val collate_src_chunks =
  collate_chunks o (List.filter (not o Chunk.is_example));

(* TODO: flatten ("untangle"?) the chunk inclusions *)
