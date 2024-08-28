(**
Tangle extracts source code from literate markdown.

Right now, this will look for all the codefenced blocks, remove
the ones without a language, remove the ones which are
considered "example" blocks (which end the language name in an
asterisk), then concatenate them all together in order.

WEB-like named code chunks are not supported.

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
      val len = size s
    in
      case (CharVector.findi (fn (i,c) =>
                                 (i >= pos) andalso
                                 (0 = i orelse
                                  #"\n" = String.sub(s,i-1)) andalso
                                 #"`" = c andalso
                                 (len > (pos + 2)) andalso
                                 (#"`" = String.sub(s, i + 1)) andalso
                                 (#"`" = String.sub(s, i + 2)))
                             s) of
          NONE => NONE
        | SOME(i,c) => SOME(i)
    end;

(*
harvest_code_block : string -> int -> (substring * int) option

Given a literate markdown content string, and a given starting
position, find the next code block which occurs. If none are
left, then `NONE` is returned.

But if one is found, then its contents (i.e., everything BETWEEN
the three backticks, but NOT INCLUDING the backtick delimiters)
are returned as a `substring option` as well as the position
immediately after the closing three backticks.
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
substring_next_newline : substring -> int -> int option

Determine the next newline character's position which occurs in
the given substring, the first one occurring AFTER the given
position. 
*)
fun substring_next_newline s pos =
    case (CharVectorSlice.findi (fn (i,c) => i > pos andalso
                                             #"\n" = c) s) of
        NONE => NONE
      | SOME(i,c) => SOME i;

(*
gather_code : string -> substring list

Extracts everything between the code fences, i.e., after the
initial `` ``` `` and before the terminal code fence ---
INCLUDING the metadata [language, etc.].
 *)
fun gather_code (s : string) =
    let
      val init : substring list = [];
      fun gather_iter (pos : int) (acc : substring list) =
          case (harvest_code_block s pos) of
              NONE => acc
            | SOME(code,i) => gather_iter i (code::acc);
    in
      gather_iter 0 init
    end;

(* get_chunks : string -> Chunk.t list

Extract the chunks of code present in a string representing a
Markdown file. This collects all codefenced blocks, extracts
the metacode to accompany each Chunk object. *)
fun get_chunks s =
  let
    val blocks = gather_code s;
  in
    map ((Chunk.from) o (Metadata.from_codefence_block)) blocks
  end;

(* form_src : string -> string

For a string representing a Markdown file, produce a string
consisting of all the code in the file.
 *)
fun form_src s =
  let
    val chunks = List.filter (fn chunk =>
                                 Chunk.has_language chunk andalso
                                 not (Chunk.is_example chunk))
                             (get_chunks s);
  in
    Substring.concat (map Chunk.code chunks)
  end;

(* collate_chunks : Chunk.t list -> Chunk.t list list

Produces a collection of a list of chunks in the same file,
presented in the same order as given. *)
local
  fun name chunk = Metadata.get (Chunk.metadata chunk)
                                "name";
  (* Take the initial sublist of items which share the same name *)
  fun take_with_names n [] acc = (rev acc, [])
    | take_with_names n (chunks as (c::cs)) acc =
      if name c = n orelse name c = NONE
      then take_with_names n cs (c::acc)
      else (rev acc, cs);
  fun iter acc [] = acc
    | iter acc chunks =
      let
        val (cs,rest) = take_with_names (name (hd chunks)) chunks [];
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

fun collate_src_chunks chunks =
  collate_chunks (List.filter (not o Chunk.is_example) chunks);

