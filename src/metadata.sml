
structure Metadata :> METADATA = struct

type key = string;
type value = string;
datatype t = T of { language : (string option)
                  , is_example : bool
                  , kvs : (key * value) list
                  , kvs_size : int
                  };
type Metadata = t;

exception Runaway of string;

fun is_blank (s : substring) =
  CharVectorSlice.all (Char.isSpace) s;

(* parse_language : substring -> int -> (string * bool)

Determine the language and whether this is an example code
block. The language will be a string object without a star
suffix.
*)
fun parse_language (s : substring) idx =
  let
    val lang = Substring.slice(s, 0, SOME idx)
  in
    if Substring.isSuffix "*" lang
    then (Substring.string(Substring.slice(s,0,SOME(idx - 1)))
         , true)
    else (Substring.string lang, false)
  end;

fun trim_substr_leading s =
  Substring.dropl Char.isSpace s;

fun trim_substr_trailing s =
  Substring.dropr Char.isSpace s;

fun trim_substr s =
  trim_substr_trailing (trim_substr_leading s);

(* extract_kvs : substring -> int -> ((key * value) list) * int
*)

fun extract_kvs s start =
  let
    val len = Substring.size s;
    fun end_quote i = (fn (j,c) =>
                         j > i andalso
                         #"\"" = c andalso
                         #"\\" <> Substring.sub(s,j-1));
    fun end_apostrophe i = (fn (j,c) =>
                               j > i andalso
                               #"'" = c);
    fun end_delim i = if #"\"" = Substring.sub(s,i)
                      then end_quote i
                      else if #"'" = Substring.sub(s,i)
                      then end_apostrophe i
                      else (fn (j,c) =>
                               j > i andalso
                               (#"=" = c orelse
                                #"," = c orelse
                                #"}" = c));
    fun delim_len i = if #"\"" = Substring.sub(s,i) orelse
                         #"'" = Substring.sub(s,i)
                      then 1
                      else 0;
                      
    fun extract_entry i =
      if len <= i
      then (if not (Substring.isSuffix "}" s)
            then raise Runaway "Runaway key-value metadata in code block"
            else (Substring.full "", len))
      else if Char.isSpace (Substring.sub(s,i))
      then extract_entry (i + 1)
      else (case CharVectorSlice.findi (end_delim i) s of
                NONE => raise Runaway "Runaway key-value metadata in code block"
              | SOME (j,c) => 
                let
                  val ell = delim_len i
                in
                  (Substring.slice(s, i + ell, SOME((j - i) - ell))
                  , j + ell)
                end);
    fun extract_iter idx acc =
      if (len <= idx) 
      then (acc, Int.min(idx + 1, len))
      else if idx > 0 andalso (#"}" = Substring.sub(s,idx - 1))
      then (acc, Int.min(idx, len))
      else let
        val (ent,i) = extract_entry idx;
        val entity = trim_substr ent;
        val k = Substring.string entity;
      in
        if len > i andalso #"," = Substring.sub(s,i)
        then extract_iter (i + 1) ((k, k)::acc)
        else if len > i andalso #"}" = Substring.sub(s,i)
        then ((k, k)::acc, Int.min(i + 1, len))
        else if len > i andalso #"=" = Substring.sub(s,i)
        then let val (rhs,j) = extract_entry (i + 1);
                 val v = (Substring.string o trim_substr) rhs;
             in (case (CharVectorSlice.findi
                           (fn (ix,ch) =>
                               ix >= j andalso
                               not (Char.isSpace ch))
                           s) of
                     SOME (j2,cc) => extract_iter (j2+1) ((k,v)::acc)
                  | NONE => ((k,v)::acc, len - 1))
             end
        else raise Runaway (concat ["Runaway metadata starting at"
                                   , (Int.toString start)])
      end;
  in
    extract_iter start []
  end;


(* parse_kvs : substring -> int -> ((key * value) list) * int

Given a substring and where the language ends, look for metadata
presented as "{key=val, ...}".

If there is any metadata, return a list of the key-value pairs
**and** where the metadata ends.

If there is no metadata, this just returns the empty list and
the given index.

If there is a brace "{" found but it is NOT on the same line as
the language, then we must assume this is part of the source
code contained in the fence block. We therefore treat it the
same as the previous case (i.e., as if there were no metadata). 
 *)
fun parse_kvs s lang_ends_idx =
  case CharVectorSlice.findi (fn (i,c) =>
                                 i > lang_ends_idx andalso
                                 #"{" = c)
                             s of
      NONE => ([], lang_ends_idx)
    | SOME (start, _) =>
      (case CharVectorSlice.find (fn (c) => #"\n" = c)
                                 (Substring.slice(s,0,SOME start)) of
           SOME _ => (* The "{" began on a newline,
                        it is not metadata! *)
           ([], lang_ends_idx)
         | NONE => (* I can't remember what this concern was about...
 if Substring.size <= start + 1
                   then (* stranger things have happened... *)
                     ([], start)
                   else
                   *)
                     extract_kvs s (start + 1)); 

(* from_codefence_block : substring -> Metadata * substring

ASSUME: the substring starts immediately **AFTER** the three
backticks demarcating the start of a code block.

ENSURES: if there is no metadata, the substring returned is
identical to the given substring.
 *)
fun from_codefence_block b =
  (case CharVectorSlice.findi (fn (i,c) =>
                                  Char.isSpace c orelse
                                  #"{" = c)
                              b of
       NONE => (T { language=NONE
                  , is_example=false
                  , kvs=[]
                  , kvs_size=0
                  },
                b)
     | SOME (0,_) =>
       let
         val (kvs,j) = parse_kvs b 0;
       in
         (T { language = NONE
            , is_example = true
            , kvs = kvs
            , kvs_size = length kvs
            },
          (Substring.slice(b, j, NONE)))
       end
     | SOME (i,_) => if is_blank(Substring.slice(b,0,SOME i))
                     then (T { language=NONE
                             , is_example=false
                             , kvs=[]
                             , kvs_size=0
                             },
                           b)
                     else
                       let
                         val (lang,is_ex) = parse_language b i;
                         val (kvs,j) = parse_kvs b i;
                       in
                         (T { language = SOME lang
                             , is_example = is_ex
                             , kvs = kvs
                             , kvs_size = length kvs
                             },
                          (Substring.slice(b, j, NONE)))
                       end);

(* language : Metadata -> string option *)
fun language (T {language=lang,...}) = lang;

(* is_example : Metadata -> bool *)
fun is_example (T {is_example=is_ex,...}) = is_ex;

(* lookup : (key * value) list -> key -> value option *)
fun lookup [] _ = NONE
  | lookup ((key,value)::kvs) k =
    if key = k
    then SOME value
    else lookup kvs k;

(* get : Metadata -> key -> value option *)
fun get (T {kvs,...}) k = lookup kvs k;

fun dbg (T {kvs,...}) =
  "kvs = ["^(concat (map (fn (k,v) => ("("^k^","^v^")")) kvs))^"]";

end;
