---
title: Metadata Parsing
---

# Metadata Parsing

## Problem Statement

The basic problem is that Markdown allows the first line of a code
block to include an optional "info string":

```md*
  ﻿`﻿`﻿`info
  code code code
  code code code
  code code code
  ﻿`﻿`﻿`
```

We will parse the info string for usage. Specifically the infostring
will consist of:

1. nothing --- we just have three backticks followed by a "newline";
2. a language name followed by a "newline";
3. a language name followed by at least one whitespace then a
   key-value map delimited by `{` and `}`.
   
The key-value map can specify the output file (if we want to write,
e.g., to a signature file as an aside to the rest of the module).

Keys and values, for us, are just strings.

The syntax for key-value entries should be `key = value`
where the key (or the value, or both) can be in single quotes, or in
double quotes (with escaped double quotes written as `\"`), or as a
sequence of characters excluding "=" and "," and "}" all without
leading or trailing whitespace.

If a key-value pair is "just a key" (e.g., `{foo}`), then it is
treated as `{foo=foo}`.

For keys/values which start in a quote but do not end in a quote, then
this is a "runaway". The Metadata parser should raise a `Runaway`
exception.

### Other keys and values

We want to keep track of the language for the code block.

There's a "hack" we support treating a suffix asterisk to the language
name (like `sml*`, `md*`, `html*`, etc.) flags the code block as
"non-inclusive" --- for example, as code examples, or attempted
solutions which deserve analysis/discussion.

Towards this end, the `Metadata` should have a predicate telling us if
it describes an _example_ or not.

Similarly, it's useful for the `Metadata` to tell us what the
programming language the block's infostring states.

## Sketch of the Basic Contracts

We want to describe the Metadata as, basically, an associative array
of some sort.

Like all signatures describing an abstract data structure, we need a
`type t` describing the structure. For ease of readability, we have a
type synonym `type Metadata = t` for the function signatures to be
more clear.

As mentioned briefly in passing, we're treating the keys and values as
ordinary strings.

We also need a special exception `Runaway` to be raised if the key (or
value) is a runaway string.

```sml {file=metadata.sig}
signature METADATA = sig
  type t;
  type Metadata = t;
  type key = string;
  type value = string;
  exception Runaway of string;
```

The second half of the signature are the public-facing function
signatures.

We need a way to construct a new Metadata object from a substring
describing a code fence block's contents. It should return a
`Metadata` object **and** the substring describing the rest of the
code fence block's contents starting immediately after the
end of the metadata.

The remaining functions are what programmers call "accessor methods",
ways to obtain information stored in a Metadata object without
modifying the Metadata object.

There is no way to _modify_ a Metadata object. This is
intentional. Reasoning about objects which can mutate is hard, and we
don't get bonus points for making life harder for ourselves.

```sml
  val from_codefence_block : substring -> Metadata * substring;
  val language : Metadata -> string option;
  val is_example : Metadata -> bool;
  val get : Metadata -> key -> value option;
  
  val same_name : Metadata -> Metadata -> bool;
  val has_no_name : Metadata -> bool;
  (* get the key-value pairs as a string, for debugging purposes *)
  val dbg : Metadata -> string;
  
  val eq : Metadata -> Metadata -> bool;
end;
```

# Implementing the Metadata module

## Basic types

The basic types needed to describe a Metadata object are already
mentioned: keys are strings, values are strings, `Metadata` is just a
type synonym, and `Runaway` is an exception whose payload is a string
message.

The `Metadata.t` type stores the relevant data parsed from a
particular code fence block's infostring: the language, if it's an
"example", and the key-value pairs in the braces (if any).

```sml {file=metadata.sml}
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
```

## Looking up the value of a key

When looking up a key in the `kvs` list, there are two cases we could
encounter:

1. the key-values list is empty `kvs = []`, in which case the value
   associated with the given key `k` would be `NONE`;
2. the key-values list is nonempty `(key,value)::kvs`, in which case
   we have to test if `key` is equal to the given key `k`; if so, then
   return the associated value wrapped in an option `SOME value`;
   if not, then we should recursively check the rest of the key-value
   entries.
   
Given a list of key-value pairs ("haystack"), and a key ("needle"),
let us return the associated value found in the key-value pairs (if any).

```sml
(* lookup : (key * value) list -> key -> value option *)
fun lookup [] _ = NONE
  | lookup ((key,value)::kvs) k = if key = k
                                  then SOME value
                                  else lookup kvs k;
```

The function to `get` a value associated to a given key `k` in a
`Metadata` object is just a wrapper around this function.

```sml
(* get : Metadata -> key -> value option *)
fun get (T {kvs,...}) k = lookup kvs k;
```

## Constructing a new Metadata object

Given a substring (of some input text) which contains the contents of
a code fence block (i.e., everything **after** the first three
backticks `` ``` `` up to but **not including** the final three
backticks), we want to find out the language (if any) and the
key-value data (if any).

We have no guarantee of what the input will look like: there may be
some leading whitespace, there may not.

The first step will be to trim the first line of all leading
whitespace --- but **not** the `\n` character ending the first line.

What happens if _somehow_ there is no non-whitespace character and
there is no newline character? In that bizarre situation, we should
just return the input back unchanged.

```sml
fun is_blank (s : substring) =
  CharVectorSlice.all (Char.isSpace) s;

fun trim_firstline (s : substring) = 
  (case CharVectorSlice.findi (fn (i,c) =>
                                  not (Char.isSpace c) orelse
                                  #"\n" = c)
                              s of
       NONE => s
     | SOME(i,_) => Substring.slice(s,i,NONE));
```

Now, assuming we've been given a cleaned up substring with no leading
whitespace in the first line, we expect it to either begin with the
language name or possible with braces.

Further, we want to take care
of the edgecase where the first line is blank **before** moving on to
the cleaned up case. We'll just return the `empty_metadata` constant,
private to the module, which stores the situation when there is no
metadata in a block --- this code block should not be treated as a
chunk, it should not be exported, it's just some "example" text
presented in monospace font.

We want to find the first whitespace (separating the language from the
key-value metadata) or opening brace `{` (starting the key-value
metadata). 

Well, let us define the empty metadata constant:

```sml
val empty_metadata = T { language = NONE
                       , is_example = true
                       , kvs = []
                       , kvs_size = 0
                       };
```

### More helper functions to trim whitespace

I know we will need to trim whitespace...eventually, so let us get the
trimming functions out of the way now.

By "trimming whitespace", I mean roughly "removing whitespace from a
substring". Really, I mean returning a _new_ substring which skips the
whitespace. 

We can trim leading whitespace (e.g., transforming `"    foo bar spam  "`
into `"foo bar spam  "`) and trailing whitespace (e.g., transforming
`"    foo bar spam  "` into `"    foo bar spam"`).

Fortunately, the
Standard Basis library has the ability to split a substring into two
substrings based on the first instance when a predicate `char -> bool`
fails. Even more fortunate, the library provides functions for taking
the first (or second) substring based on splitting starting from the
left (or right). We can use these functions to implement the trimming
leading and trailing whitespace from a substring:

```sml
fun trim_substr_leading s =
  Substring.dropl Char.isSpace s;

fun trim_substr_trailing s =
  Substring.dropr Char.isSpace s;
```

Then trimming both the leading _and_ trailing whitespace from a
substring amounts to composing these functions together.

Remember, Standard ML uses `o` for the composition operator (because
it is the closest thing a typewriter could produce resembling the
mathematical symbol $\circ$).

```sml
val trim_substr = trim_substr_leading o trim_substr_trailing;
```

Last, we may want to find the first non-whitespace character in a
substring occurring **after** a given position.

The trick is to slice the substring **from** that given position, find
the first whitespace character, then add the two positions together.

If no non-whitespace character may be found, we should return the
length of the original substring. In effect, the "end of string"
position is treated "as if" it were whitespace.

```sml
fun find_non_ws (s : substring) i =
  case CharVectorSlice.findi (fn (_,c) =>
                                 not (Char.isSpace c))
                             (Substring.slice(s, i, NONE)) of
      NONE => Substring.size s
    | SOME (j,c) => i + j;
```

### Parsing the language name

Assuming that we have found the _end_ position of the name for the
programming language used, we should extract the language name.

Remember, we are allowing the user to add an optional asterisk suffix
to indicate that the language is used for an example code snippet.

So we should return (1) the language name without this trailing
asterisk, and (2) a Boolean indicating if it's an example or not.

```sml
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
```

### Helper functions for raising Runaway exceptions

We should probably want to tell the user that there was a runaway
key-value pair **somewhere** on line X in the supplied input.
That'd be nice.

We can get the line number for a substring by, well, examining the
string it is drawn from. Remember, in effect, Standard ML implements a
`substring` as an ordered triple consisting of (1) a pointer to the
string it's a substring of, (2) the start of the substring, and (3)
the length of the substring.

We can obtain this data back, then form the substring of the parent string
starting at the 0 position and all the way up to the start of the
given substring, then counting the number of times the newline
character has been encountered.

Now, if hypothetically the substring given were just the first line of
the Markdown input, then we should return `1` for the line number. However,
there are zero `#"\n"` characters which would be counted using the
scheme outlined above. This is because we described counting the
number of _line breaks_ instead of the number of lines.

Fortunately, the line number is just one more than the number of line breaks.

```sml
fun trim_substr_leading s =
  Substring.dropl Char.isSpace s;

fun trim_substr_trailing s =
  Substring.dropr Char.isSpace s;

fun trim_substr s =
  trim_substr_trailing (trim_substr_leading s);

fun find_non_ws (s : substring) i =
  case CharVectorSlice.findi (fn (_,c) =>
                                 not (Char.isSpace c))
                             (Substring.slice(s, i, NONE)) of
      NONE => Substring.size s
    | SOME (j,c) => i + j;

(* line : substring -> int

Obtain the line number of a substring, relative to the larger
[literate Markdown] source file. *)
fun line (sub : substring) =
  let
    val (s, i, sub_len) = Substring.base sub;
  in
    length (Substring.fields (fn c => #"\n" = c)
                             (Substring.extract(s, 0, SOME i)))
  end;

val line_number = (Int.toString) o line;
```

Now, raising a runaway exception in a substring at a given position
should inform the user of the line number where things went horribly
awry. 
We can do this by getting the line number for the sub-substring starting at
the given position.

This gives us the helper function for raising runaway strings:

```sml
fun runaway s i =
  let
    val num = line_number (Substring.slice(s, i, NONE))
  in
    raise Runaway ("LINE " ^
                   num ^
                   " runaway key-value metadata")
  end;
```

In practice, we should do _more_, like give the position in the line
for the runaway. But this is good enough for our purposes.

### Extracting key-value pairs

Supposing we have arrived at the position of the first key-value pair,
we want to...well...extract the metadata.

This can be done by observing that each key will have certain
delimiters (if it started with a double quote, then it must end with a
double quote; if it started with a single quote, then it must end with
a single quote; otherwise, it must end with `=` separating the key
from the value, or a `,` separating it from other keys, or `}` ending
the metadata key-value pairs altogether).

It's important to remember, we allow escaped double quotes, but not
escaped single quotes. This was an arbitrary choice, but it seems to
be the convention with other languages these days.

```sml
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
```

Now, we have the first major helper function to help extract either a
"key" or a "value" starting at index `i`. The logic is identical
regardless of which it is:

- If the index `i` is longer than the substring's length, then we're
  possibly out of bounds. The only saving grace is if the substring
  ended with a close brace `}`. Otherwise, raise a runaway exception
  error.
- If the character at position `i` of the substring is a whitespace,
  then skip to the first non-whitespace character appearing after `i`
  in the substring and start over from there.
- Otherwise, try finding the "end delimiter" after position `i` in the
  substring. If there is none, then we've got a runaway situation
  which should be flagged. Otherwise, the ending delimiter is found at
  position `j` and we should carve out the substring for the entry
  (key/value) excluding the delimiter.
  
This logic is straightforward and we can check mentally a few examples
to see this does what we hope for.

```sml
    (* extracting a "key" or "value" is the same logic, which
       is described here as `extract_entry` *)
    fun extract_entry i =
      if len <= i
      then (if not (Substring.isSuffix "}" s)
            then runaway s i
            else (Substring.full "", len))
      else if Char.isSpace (Substring.sub(s,i))
      then extract_entry (find_non_ws s i)
      else (case CharVectorSlice.findi (end_delim i) s of
                NONE => runaway s i
              | SOME (j,c) => 
                let
                  val ell = delim_len i
                in
                  (Substring.slice(s, i + ell, SOME((j - i) - ell))
                  , j + ell)
                end);
```

Now, we want to create an iterator function which will accumulate
key-value pairs until we're done with the metadata. (Or, we have a
runaway metadata problem, whichever occurs first.)

The flow of logic is remarkably similar to the situation where we
extracting an "entry" in the key-value pairs:

- If the position for the next key-value pair is greater than the
  length of the substring (representing the code block), then we raise
  a runaway exception.
- If the position for the next key-value pair is positive and the
  character located there is equal to the close brace, then we're
  done. We should return the accumulated list of key-value pairs and
  the position of the close brace (or the length of the substring,
  whichever is smaller).
- Otherwise, we get to the interesting part: there's work to be done. 
  1. Extract the key using the `extract_entry` invoked at the given
     position. This will give us the key as a substring _and_ the
     position in the substring immediately after the key. We trim the
     whitespace from the key, and turn it into a proper string.
  2. Now we try to find the value. This itself decomposes into several
     cases.
     1. We have not exhausted the substring describing the code block,
        and we have run into a comma. This happens with `{foo, key = value,...}`
        and we decided to treat this as `{foo = foo, ...}`.
        Then we iterate, calling `extract_iter` again but with the
        position of the next character updated and pushing `(k,k)` on
        the accumulator
     2. We have not exhausted the substring and we have run into the
        closing brace `}` for the metadata. Then we just do as before,
        treat the value as equal to the key, but we terminate the
        function returning `(k,k)::acc` and the location where the
        closing brace occurred.
     3. If we have not exhausted the substring and we have found an
        equal sign as the delimiter, then we have a value on the
        right-hand side of the equality. We extract the entry
        starting at the position immediately after the equality sign,
        trim its leading/trailing whitespace, then store this as a
        string --- it's the value for the given key.
        We find the next non-whitespace character in the metadata,
        and then we iterate (updating the accumulated
        key-value pairs by pushing the newly extracted pair onto it).
     4. Otherwise, we don't have a value for the key, which means
        we've run into an exceptional situation. We raise the runaway
        exception and bail out!

```sml
    (* extract_iter will accumulate a key-value pair until
       we're done, or we have a runaway exception thrown. *)
    fun extract_iter idx acc =
      if (len <= idx) 
      then runaway s 0
      else if idx > 0 andalso (#"}" = Substring.sub(s,idx - 1))
      then (acc, Int.min(idx, len))
      else let (* step 1: extract key *)
        val (ent,i) = extract_entry idx;
        val entity = trim_substr ent;
        val k = Substring.string entity;
      in 
        (* step 2, case 1: there is no value *)
        if len > i andalso #"," = Substring.sub(s,i)
        then extract_iter (i + 1) ((k, k)::acc)
        (* step 2, case 2: there is no value AND we're done *)
        else if len > i andalso #"}" = Substring.sub(s,i)
        then ((k, k)::acc, Int.min(i + 1, len))
        (* step 2, case 3: extract value *)
        else if len > i andalso #"=" = Substring.sub(s,i)
        then let val (rhs,j) = extract_entry (i + 1);
                 val v = (Substring.string o trim_substr) rhs;
             in
               extract_iter (1 + find_non_ws s j) ((k,v)::acc)
             end
        (* step 2, case 4: raise runaway exception *)
        else runaway s start
      end;
```

Now, the main part of the `extract_kvs` function: we invoke the
`extract_iter` at the start of the first key, with the empty list as
its initial key-value accumulator.

```sml
  in
    extract_iter start []
  end;
```

### Parsing the key-value entries

Just as we had `parse_language` which takes the code fenced block's
contents (as a substring) and the position of the first whitespace
_after_ the language name; we should have a `parse_kvs` which takes
the code fence block and the same position (immediately after then
language name) and try to parse the metadata for key-value pairs.

If there is any metadata, we should return the key-value pairs as a
list **and** the position where the metadata ends.

If there is no metadata, then we should just return the empty list and
the position given as an argument.

We should assume if there is a brace `{` found but it is not on the
same line as the language, then it's part of the source code --- i.e.,
it's part of the _content_ (the actual data) and not the metadata.
Therefore, we should treat this situation as the previous case (return
the empty list and the position given as an argument).

```sml
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
         | NONE => extract_kvs s (start + 1)); 
```

### Constructing Metadata: Putting it all together

We have all the parts to describe how we parse out Metadata from a
codeblock.

First, we have a helper function which describes the situation which
we expect to encounter: we have the code block be a substring who
leading character is not a whitespace character (and in particular,
it has a nonempty first line).

If there is no whitespace or `{` in the code block substring, then we
have no metadata.

If there is whitespace or `{` to be found in the zeroth position
(i.e., starting the substring), then it must be because of the `{`
since we assumed the leading character is not whitespace. This means
there is no language (which means we should treat this block as an
example), just metadata. So we should just parse the 
metadata, and return the ordered pair consisting of this parsed
metadata and the substring immediately after the end of the metadata.

If there is a whitespace or `{` character located at position `i > 0`,
then we have a language to parse and possibly some metadata to parse.
If, somehow, we have characters between 0 and `i` be all blank, we
should then --- well, how could that happen? We should raise an
exception, but we'll just pass the empty metadata back.

Otherwise, we parse the language, parse the metadata, and use these to
construct the Metadata object.

Of particular note, if the user has passed in `example = true` (or
even just `{...,example,...}`), then we should treat the code block as
an example. This will override anything inferred from an asterisk
suffixed language name.

```sml
fun from_clean_codefence_block b =
  (case CharVectorSlice.findi (fn (i,c) =>
                                  Char.isSpace c orelse
                                  #"{" = c)
                              b of
       NONE => (empty_metadata, b)
     | SOME (0,_) =>
       (* Since it starts with nonwhitespace, this must be a
          `#"{"` *)
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
                     then (empty_metadata, b)
                     else
                       let
                         val (lang,is_e) = parse_language b i;
                         val (kvs,j) = parse_kvs b i;
                         val is_ex = is_e orelse
                                     (case lookup kvs "example" of
                                          SOME v => "false" <> v
                                        | NONE => is_e);
                       in
                         (T { language = SOME lang
                            , is_example = is_ex
                            , kvs = kvs
                            , kvs_size = length kvs
                            },
                          (Substring.slice(b, j, NONE)))
                       end);
```

Now we have the constructor proper. We begin by taking the substring
describing the contents of the code fence block, trim the first line
to remove leading whitespace. This is the cleaned up block.

If the first line of the cleaned up block is empty, then there is no
metadata, so return the `empty_metadata` and the initial block.

Otherwise, we pass the cleaned up substring to the
`from_clean_codefence_block` function we just constructed.

This is our "smart" constructor, which will check if the metadata is
malformed and raise appropriate exceptions alerting the user that
things have gone awry.

```sml
(* from_codefence_block : substring -> Metadata * substring

ASSUME: the substring starts with the first nonwhitespace
character on the first line immediately **AFTER** the three
backticks demarcating the start of a code block.

ENSURES: if there is no metadata, the substring returned is
identical to the given substring.
 *)
fun from_codefence_block block =
  let
    val b = trim_firstline block;
  in
    if Substring.isPrefix "\n" block
    then (empty_metadata, block)
    else from_clean_codefence_block b
  end;
```

## Accessor Functions

The remaining accessor functions are as we would expect. Get the
language simply returns the language.

```sml
(* language : Metadata -> string option *)
fun language (T {language=lang,...}) = lang;
```

Asking if the metadata describes example code should simply return
whatever value has been parsed from the language name.

```sml
(* is_example : Metadata -> bool *)
fun is_example (T {is_example=is_ex,...}) = is_ex;
```

We also want to test for two chunks to share the same name. This
refers to the fields stored in the `"file"` and `"name"` fields of the
metadata key-value store.

If neither one is present, then the chunk is considered to have "no
name".

```sml
fun same_name lhs rhs =
  let
    fun name (this : Metadata) = ( get this "file"
                                 , get this "name");
  in
    (name lhs) = (name rhs)
  end;

fun has_no_name this =
  (NONE = get this "file") andalso
  (NONE = get this "name");
```

And for debugging purposes, we might want to see the list of key-value
pairs present.

```sml
fun dbg (T {kvs,...}) =
  "kvs = ["^(concat (map (fn (k,v) => ("("^k^","^v^")")) kvs))^"]";
```

And before we forget, we should have an equality predicate to see if
we're describing the _same_ metadata. This amounts to checking the
languages are both absent or they're both present and equal to each
other, **and** they are both examples (or both are not examples),
**and** the key-value pairs are identical.

```sml
fun eq (lhs as T {kvs=kvs_lhs,...}) (rhs as T {kvs=kvs_rhs,...}) =
  (language lhs = language rhs) andalso
  (kvs_lhs = kvs_rhs) andalso
  (is_example lhs = is_example rhs);
```

...and that's it!

```sml
end;
```

