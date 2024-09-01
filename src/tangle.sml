structure Tangle = struct

  (* usage : unit -> unit

  When no arguments are given, or a "--help" flag is passed,
  print out the recommended usage for this program.
   *)
  fun usage () =
    (print o (String.concatWith "\n"))
["tangle-md --- extracts source code from a literate Markdown file"
,""
,"Optional arguments:"
,"--extract <file> to extract all source code associated with"
,"    specified file or files (can appear multiple times or be"
,"    comma-separated WITHOUT space)."
,""
,"--output <path>, --output=<path> will produce the source code"
,"    in the specified directory, or in the specified file."
,"    This WILL overwrite the file's contents."
,""
(* (* I don't think recursive is a good idea at the moment... *)
,"--r <dir>, --r=<dir> will recursively read all code chunks"
,"    from all the literate Markdown files found in the directory"
,"    and all descendent subdirectories. Can be comma-separated"
,"    for multiple directories.\n"
,"    If given both --r and --extract, then it will read all"
,"    literate Markdown files and then extract all the chunks"
,"    which specify they will be written to the given file or files."
,""
*)
];

fun trim_substr_leading s =
  Substring.dropl Char.isSpace s;

fun trim_substr_trailing s =
  Substring.dropr Char.isSpace s;

fun trim_substr s =
  trim_substr_trailing (trim_substr_leading s);

fun string_trim s =
  (Substring.string o trim_substr o Substring.full) s;

(*
partition_arguments : string ->
                      string list ->
                      (string list, string list)

Given the command line arguments and a flag which is supposed to
be followed by a parameter (like "--css dir"), partition the
arguments into `(flags, everything else)`.

The `rest` will remain in the order passed.

It will also handle cases like "--css=dir" or `<flag>=<value>`
correctly treating it as "-css dir" (respectively `<flag> <value>`).

Comma-separated values are supported provided there is NO
WHITESPACE separating them. So `val1,val2,val3` is OK, but
`val1, val2,    val3` is NOT.
*)
fun partition_arguments (flag : string) args =
  let
    fun get_val x =
      String.extract(x, String.size(flag)+1, NONE);
    fun add_val v coll =
      if String.isSubstring "," v
      then (map string_trim
                (String.tokens (fn c => #"," = c) v)) @ coll
      else v::coll;
    fun opt_part_iter (flags,rest) [] = (flags, rev rest)
      | opt_part_iter (flags,rest) (x::[]) =
        if String.isPrefix (flag ^ "=") x
        then (add_val (get_val x) flags, rev rest)
        else (flags, rev (x::rest))
      | opt_part_iter (flags,rest) (x::y::xs) =
        (if flag = x
         then opt_part_iter (add_val y flags,rest) xs
         else if String.isPrefix (flag ^ "=") x
         then opt_part_iter (add_val (get_val x) flags, rest) (y::xs)
         else opt_part_iter (flags,x::rest) (y::xs))
  in
    opt_part_iter ([], []) args
  end;

fun extract_opt (opts : string list) =
  partition_arguments "--extract" opts;

fun recursive_opt (opts : string list) =
  partition_arguments "--r" opts;

(* output_opts : string list -> (string list, string list)

Returns an at most singleton in the first component.

If there are multiple "--output" options passed into the
command line arguments, then this will exit the program
immediately with failure.
 *)
fun output_opt (opts : string list) =
  case partition_arguments "--output" opts of
      (x::y::xs, rest) =>
          (print("ERROR: multiple outputs passed in\n");
           print("       whereas a maximum of 1 --output allowed.\n");
           print("BAILING OUT!\n");
           OS.Process.exit OS.Process.failure
          )
    | (out, rest) => (out, rest);
  
(* read_file : string -> string

Given a file, try reading the contents.

If an exception occurs, close the input stream.

ENSURES: No input stream remains open after reading in a file.
*)
fun read_file path =
  let
    val inStream = TextIO.openIn path
              handle (e as IO.Io _) => (print ("IO Exception raised while trying to open "^path^"\n");
                                        raise e);
    val txt = TextIO.inputAll inStream
              handle (e as IO.Io _) => (TextIO.closeIn inStream;
                                        print ("IO Exception raised while trying to read "^path^"\n");
                                        raise e)
                   | (e as Size) => (TextIO.closeIn inStream;
                                     print ("Size Exception raised while trying to read "^path^"\n");
                                     raise e);
  in
    TextIO.closeIn inStream;
    txt
  end;

(* write_to_file : string -> string -> unit

Writes to `output_name` the given `contents`, overwriting
whatever might already exist in the file.

REQUIRES: `output_name` is the (absolute or relative) path to
the file to be written.

RAISES: IO.Io exception if the program cannot write to the
specified `output_name` path.

ENSURES: no output stream remains open after this function has
finished.
 *)
fun write_to_file output_name contents =
  let
    val outStream = TextIO.openOut output_name
              handle (e as IO.Io _) => (print ("IO Exception raised while trying to write to "^output_name^"\n");
                                        raise e);
  in
    TextIO.output(outStream, contents);
    TextIO.flushOut outStream;
    TextIO.closeOut outStream
  end;

local
  fun read_all_iter acc [] = String.concatWith "\n\n" acc
    | read_all_iter acc (path::ps) =
      read_all_iter ((read_file path)::acc) ps
in
val read_all = read_all_iter []
end;

(* mkDir : string -> string

Creates a directory and, if necessary, any parent directories
(assuming the program has access to do so).

Returns the full path of the directory.
*)
fun mkDir path =
  let
    fun is_dir p =
      OS.FileSys.isDir p
      handle _ => false;
  in
    if is_dir path
    then OS.FileSys.fullPath path
    else let
      val parent = OS.Path.getParent path;
    in
      mkDir parent;
      OS.FileSys.mkDir path;
      OS.FileSys.fullPath path
    end
  end;

(* output_file : string -> string -> string

Determine the output file's name given the `input_name` of the
input file and the `output_path` for the directory where we wish
to produce the output.

WARNING: this will OVERWRITE any pre-existing files sharing
the output name.
*)
fun output_file file_name "" =
    file_name
  | output_file file_name dir_path =
    let
      val _ = print ("Trying to create "^dir_path^"\n");
      val p = mkDir dir_path;
    in
      OS.Path.fromUnixPath
          ((OS.Path.toUnixPath p) ^"/"^
           (OS.Path.file file_name))
    end;

fun write_for chunks output_dir name =
  let
    val cs = List.concat
                 (List.filter (fn [] => false
                              | c::cs => (case (Metadata.get
                                                    (Chunk.metadata
                                                         c) "file") of
                                              SOME n => n = name
                                            | NONE => false))
                              chunks);
    fun get_n (c : Chunk.t) = case Metadata.get (Chunk.metadata c) "file" of
                                  SOME x => x
                                | NONE => "***NONE??***";
    val code = Substring.concat (map Chunk.code cs);
  in
    write_to_file (output_file name output_dir) (code^"\n")
  end;
       
fun run args =
  let
    val (output_arg, r0) = output_opt args;
    val output = case output_arg of
                     [] => ""
                   | out::_ => out;
    val (extract, rest) = extract_opt r0;
    (* val (recursive, rest) = recursive_opt r1; *)
    val ins = read_all rest;
    val chunks = (Parser.parse ins);
  in
    app (write_for chunks output) extract
    (* write_to_file output_name contents *)
  end;

(* main : unit -> unit

Run the main program, expecting files to be passed in by the
command line options, and output in a specified directory.
 *)
fun main () =
  let
    val args = CommandLine.arguments();
  in
    if null args
    then usage()
    else run args;
    OS.Process.exit(OS.Process.success)
  end;

end; (* structure Tangle *)

val main = Tangle.main;
