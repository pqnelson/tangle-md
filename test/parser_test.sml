fun mk_tangle_test name (expected : string) (input : string list)
  : Test.t =
  Test.new name (fn () =>
                    let
                      val actual = (Parser.export_src_chunks o
                                    concat) input;
                      val msg = concat ["EXPECTED: "
                                       , expected
                                       , "\n## ACTUAL: "
                                       , actual
                                       , "\n"];
                    in
                      Assert.!! msg (expected = actual)
                    end);

val tangle_test1 =
  mk_tangle_test
    "tangle_test1"
    (concat ["\nfun has_language (s : substring) =\n"
            ,"    Substring.size(s) > 0 andalso\n"
            ,"    not (Char.isSpace (Substring.sub(s,0)));\n"])
    ["This is an example\n"
    ,"of some blah blah blah\n"
    ,"```sml\n"
    ,"fun has_language (s : substring) =\n"
    ,"    Substring.size(s) > 0 andalso\n"
    ,"    not (Char.isSpace (Substring.sub(s,0)));\n"
    ,"```\n"
    ,"\n"
    ,"This function checks if there is a language\n"
    ,"for the code block\n"
    ,"And this is how we use it:\n"
    ,"\n"
    ,"```sml*\n"
    ,"extract_src ex1;\n"
    ,"```\n"];

val tangle_test2 =
  mk_tangle_test
    "tangle_test2"
    ""
    ["This consists only of example code\n"
    ,"so nothing should be extracted\n"
    ,"```sml*\n"
    ,"fun has_language (s : substring) =\n"
    ,"    Substring.size(s) > 0 andalso\n"
    ,"    not (Char.isSpace (Substring.sub(s,0)));\n"
    ,"```\n"
    ,"\n"
    ,"This function checks if there is a language\n"
    ,"for the code block\n"
    ,"And this is how we use it:\n"
    ,"\n"
    ,"```sml*\n"
    ,"extract_src ex1;\n"
    ,"```\n"];

fun chunks_eq [] [] = true
  | chunks_eq _ [] = false
  | chunks_eq [] _ = false
  | chunks_eq (c1::cs1) (c2::cs2) = (Chunk.eq c1 c2) andalso
                                    chunks_eq cs1 cs2;

fun chunks_coll_eq [] [] = true
  | chunks_coll_eq _ [] = false
  | chunks_coll_eq [] _ = false
  | chunks_coll_eq (c1::cs1) (c2::cs2) =
    chunks_eq c1 c2 andalso
    chunks_coll_eq cs1 cs2;

val collate_chunk_test1 =
  Test.new
      "collate_chunk_test1"
      (fn () =>
          let
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml {file=foo.sml}"
                                  ,"This is just some random"
                                  ,"Text captured between"
                                  ,"Three backticks"
              ]);
            val chunk1 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml {file=foo.sig}"
                                  ,"signature FOO = sig"
                                  ,"  val t;"
                                  ,"end"
              ]);
            val chunk2 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml {file=foo.sml}"
                                  ,"structure Foo :> FOO = struct"
                                  ,"  type t = unit;"
                                  ,"end;"
              ]);
            val chunk3 = (Chunk.from o Metadata.from_codefence_block) block;
            val expected = [[chunk2],[chunk1,chunk3]];
            val actual = Parser.collate_chunks [chunk1,chunk2,chunk3];
          in
            Assert.!! "collate_chunks_test1 failed"
                   (chunks_coll_eq expected actual)
          end);
    
val collate_chunk_test2 =
  Test.new
      "collate_chunk_test2"
      (fn () =>
          let
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml {file=foo.sml}"
                                  ,"This is just some random"
                                  ,"Text captured between"
                                  ,"Three backticks"
              ]);
            val chunk1 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml"
                                  ,"fun foo x ="
                                  ,"  if spam then x else bar x;"
                                  ,""
              ]);
            val chunk2 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml {file=foo.sig}"
                                  ,"signature FOO = sig"
                                  ,"  val t;"
                                  ,"end"
              ]);
            val chunk3 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml"
                                  ,"fun foo_ x ="
                                  ,"  if spam_ then x else bar__ x;"
                                  ,""
              ]);
            val chunk4 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml {file=foo.sml}"
                                  ,"structure Foo :> FOO = struct"
                                  ,"  type t = unit;"
                                  ,"end;"
              ]);
            val chunk5 = (Chunk.from o Metadata.from_codefence_block) block;
            val expected = [[chunk3,chunk4],[chunk1,chunk2,chunk5]];
            val actual = Parser.collate_chunks [chunk1,chunk2,chunk3,chunk4,chunk5];
          in
            Assert.!! "collate_chunks_test2 failed"
                   (chunks_coll_eq expected actual)
          end);
    
val collate_chunk_test3 =
  Test.new
      "collate_chunk_test3"
      (fn () =>
          let
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml {file=foo.sig}"
                                  ,"This is just some random"
                                  ,"Text captured between"
                                  ,"Three backticks"
              ]);
            val chunk1 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml"
                                  ,"fun foo x ="
                                  ,"  if spam then x else bar x;"
                                  ,""
              ]);
            val chunk2 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml {file=foo.sml}"
                                  ,"signature FOO = sig"
                                  ,"  val t;"
                                  ,"end"
              ]);
            val chunk3 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml"
                                  ,"fun foo_ x ="
                                  ,"  if spam_ then x else bar__ x;"
                                  ,""
              ]);
            val chunk4 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml"
                                  ,"structure Foo :> FOO = struct"
                                  ,"  type t = unit;"
                                  ,"end;"
              ]);
            val chunk5 = (Chunk.from o Metadata.from_codefence_block) block;
            val expected = [[chunk3,chunk4,chunk5],[chunk1,chunk2]];
            val actual = Parser.collate_chunks [chunk1,chunk2,chunk3,chunk4,chunk5];
          in
            Assert.!! "collate_chunks_test3 failed"
                   (chunks_coll_eq expected actual)
          end);

val collate_chunk_test4 =
  Test.new
      "collate_chunk_test4"
      (fn () =>
          let
            val block = Substring.full (
                String.concatWith "\n"
                                  ["md"
                                  ,"This is just some random"
                                  ,"Text captured between"
                                  ,"Three backticks"
              ]);
            val chunk0 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml {file=foo.sig}"
                                  ,"This is just some random"
                                  ,"Text captured between"
                                  ,"Three backticks"
              ]);
            val chunk1 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml"
                                  ,"fun foo x ="
                                  ,"  if spam then x else bar x;"
                                  ,""
              ]);
            val chunk2 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml {file=foo.sml}"
                                  ,"signature FOO = sig"
                                  ,"  val t;"
                                  ,"end"
              ]);
            val chunk3 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml"
                                  ,"fun foo_ x ="
                                  ,"  if spam_ then x else bar__ x;"
                                  ,""
              ]);
            val chunk4 = (Chunk.from o Metadata.from_codefence_block) block;
            val block = Substring.full (
                String.concatWith "\n"
                                  ["sml"
                                  ,"structure Foo :> FOO = struct"
                                  ,"  type t = unit;"
                                  ,"end;"
              ]);
            val chunk5 = (Chunk.from o Metadata.from_codefence_block) block;
            val expected = [[chunk3,chunk4,chunk5],[chunk1,chunk2],[chunk0]];
            val actual = Parser.collate_chunks [chunk0,chunk1,chunk2,chunk3,chunk4,chunk5];
            (*
            val _ = print ("Actual counts = ");
            val _ = app (fn coll => print ("["^
                                           (concat (map Chunk.file coll))^
                                           " "^
                                           (Int.toString
                                                    (length coll))^"]"))
                        actual;
            val _ = print "\n";
            *)
          in
            Assert.!! "collate_chunks_test4 failed"
                   (chunks_coll_eq expected actual)
          end);


Test.register_suite "parser_test" [
  tangle_test1
, tangle_test2
, collate_chunk_test1
, collate_chunk_test2
, collate_chunk_test3
, collate_chunk_test4
];
