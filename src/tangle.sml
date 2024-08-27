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

(*
is_example_block : substring -> bool

Example usage of code occurs within codefences delimited by 
an asterisked language (e.g., `` ```sml* ``), if a language is
present at all.
 *)
fun is_example_block (s : substring) =
    case substring_next_newline s 0 of
        NONE => false
      | SOME i =>
        (case CharVectorSlice.findi
                  (fn (_,c) =>
                      Char.isSpace c)
                  (Substring.slice(s,0,SOME (i + 1))) of
             SOME (j,_) => (j > 0 andalso
                            #"*" = Substring.sub(s, j - 1))
           | _ => false);

(*
has_language : substring -> bool

Given a code-fenced block as a substring, determine if it has a
language specified.
 *)
fun has_language (s : substring) =
    Substring.size(s) > 0 andalso
    not (Char.isSpace (Substring.sub(s,0)));

(*
form_src : string -> string

Forms the source code amalgamated by every code block with a
language, which are also not "example blocks". A code fenced
block is considered an "example" if its language name ends in an
asterisk.

This is almost certainly what you want to call when extracting
source code from a Markdown file.
*)
local
  fun chop_first_line (ss : substring) =
      let
        val (_, keeper) = Substring.splitl (fn c => #"\n" <> c) ss
      in
        if 0 = Substring.size keeper
        then keeper
        else Substring.triml 1 keeper
      end
in
fun form_src s =
    let
      val blocks = gather_code s;
      val blocks_with_lang = List.filter
                                 (fn b =>
                                     (has_language b) andalso
                                     not (is_example_block b))
                                 blocks;
    in
      Substring.concat
          (map chop_first_line blocks_with_lang)
    end
end;
