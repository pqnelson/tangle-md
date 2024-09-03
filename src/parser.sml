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
  rev o (map ((Chunk.from) o (Metadata.from_codefence_block))) o
  all_code_blocks;

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


(* export_all_chunks : string -> string

Given a string representation of a literate Markdown web file,
extract the Chunks --- with our without a language, which are
or are not examples --- and then flatten them into a string
for export.
 *)
val export_all_chunks =
  Substring.concat o (map Chunk.code) o
  get_chunks;

(*
List.partition implementations:

MLton
fun partition pred l =
        let
           val (pos, neg) =
              foldl (fn (x, (trues, falses)) =>
                     if pred x then (x :: trues, falses)
                     else (trues, x :: falses))
              ([], []) l
        in (rev pos, rev neg)
        end

POLY/ML
    fun partition _ [] = ([], [])
      | partition f (a::b) =
            let
            val test = f a
            and (x, y) = partition f b
            in
            if test then (a::x, y) else (x, a::y)
            end

*)

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

val collate_src_chunks =
  collate_chunks o (List.filter (not o Chunk.is_example));

(* parse : string -> Chunk.t list list

Given a string representation for a literate Markdown web,
extract the Code chunks and collate them.
 *)
fun parse (s : string) =
  let
    val results = (collate_src_chunks o get_chunks) s;
  in
    (print("Returning "^
           (Int.toString(length results))^
           " clusters of chunks\n");
     results)
  end;
end;
(* TODO: flatten ("untangle"?) the chunk inclusions *)
