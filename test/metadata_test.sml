val extract_test1 =
  Test.new "extract_test1"
           (fn () =>
               let
                 val block = Substring.full (
                     String.concatWith "\n"
                                       [""
                                       ,"This is just some random"
                                       ,"Text captured between"
                                       ,"Three backticks"
                   ]);
                 val (metadata,s) = Metadata.from_codefence_block block;
               in
                 Assert.!! (EQUAL = Substring.compare(block, s))
                        "EXPECTED true FOUND false"
               end);

val extract_test2 =
  Test.new "extract_test2"
           (fn () =>
               let
                 val block = Substring.full (
                     String.concatWith "\n"
                                       ["sml {file=example.sml}"
                                       ,"This is just some random"
                                       ,"Text captured between"
                                       ,"Three backticks"
                   ]);
                 val (metadata,actual) =
                   Metadata.from_codefence_block block;
                 val expected =
                   Substring.slice(block,
                                   size "sml {file=example.sml}",
                                   NONE);
               in
                 Assert.!! (EQUAL = Substring.compare(expected, actual))
                        ("## EXPECTED: \""^
                         (Substring.string expected)^
                         "\"\n## FOUND: "^
                         (Substring.string actual)^"\n")
                        
               end);

val extract_test3 =
  Test.new "extract_test3"
           (fn () =>
               let
                 val block = Substring.full (
                     String.concatWith "\n"
                                       ["sml {file=example.sml"
                                       ,"A runaway key-value pairing"
                                       ,"should raise a 'Fail' exception"
                   ]);
                 val (metadata,actual) =
                   Metadata.from_codefence_block block;
               in
                 Assert.!! false "EXPECTED exception thrown due to runaway metadata"
               end
               handle Metadata.Runaway msg =>
                      (Assert.!! true "FOUND exception thrown due to runaway metadata")
           );

val language_test1 =
  Test.new "language_test1"
           (fn () =>
               let
                 val block = Substring.full (
                     String.concatWith "\n"
                                       ["sml {file=example.sml}"
                                       ,"This is just some random"
                                       ,"Text captured between"
                                       ,"Three backticks"
                   ]);
                 val (metadata,_) =
                   Metadata.from_codefence_block block;
                 val expected = "sml";
                 val actual = Option.getOpt(Metadata.language metadata,"wrong");
               in
                 Assert.eq expected
                           actual
                           ("EXPECTED "^expected^"\nFOUND "^actual^"\n")
               end);

val language_test2 =
  Test.new "language_test2"
           (fn () =>
               let
                 val block = Substring.full (
                     String.concatWith "\n"
                                       ["sml* {file=example.sml}"
                                       ,"This is just some random"
                                       ,"Text captured between"
                                       ,"Three backticks"
                   ]);
                 val (metadata,_) =
                   Metadata.from_codefence_block block;
                 val expected = "sml";
                 val actual = Option.getOpt(Metadata.language metadata,"wrong");
               in
                 Assert.eq expected
                           actual
                           ("EXPECTED "^expected^"\nFOUND "^actual^"\n")
               end);

val is_example_test1 =
  Test.new "language_test1"
           (fn () =>
               let
                 val block = Substring.full (
                     String.concatWith "\n"
                                       ["sml* {file=example.sml}"
                                       ,"This is just some random"
                                       ,"Text captured between"
                                       ,"Three backticks"
                   ]);
                 val (metadata,_) =
                   Metadata.from_codefence_block block;
               in
                 Assert.!! (Metadata.is_example metadata)
                        ("EXPECTED is_example = true\nFOUND ...=false\n")
               end);

val is_example_test2 =
  Test.new "language_test2"
           (fn () =>
               let
                 val block = Substring.full (
                     String.concatWith "\n"
                                       ["sml {file=example.sml}"
                                       ,"This is just some random"
                                       ,"Text captured between"
                                       ,"Three backticks"
                   ]);
                 val (metadata,_) =
                   Metadata.from_codefence_block block;
               in
                 Assert.!! (not (Metadata.is_example metadata))
                        ("EXPECTED is_example = false\nFOUND ...=true\n")
               end);

val is_example_test3 =
  Test.new "language_test3"
           (fn () =>
               let
                 val block = Substring.full (
                     String.concatWith "\n"
                                       [""
                                       ,"This is just some random"
                                       ,"Text captured between"
                                       ,"Three backticks"
                   ]);
                 val (metadata,_) =
                   Metadata.from_codefence_block block;
               in
                 Assert.!! (Metadata.is_example metadata)
                        ("EXPECTED is_example = true\nFOUND ...=false\n")
               end);

fun mk_get_test name src k expected =
  Test.new name
           (fn () =>
               let
                 val block = Substring.full
                                 (String.concatWith "\n" src);
                 val (metadata,_) =
                   Metadata.from_codefence_block block;
                 val actual = Metadata.get metadata k;
                 val msg = (case (expected,actual) of
                                (NONE,SOME v) => ("EXPECTED: NONE\nFOUND "^v^"\n")
                              | (SOME v,NONE) => ("EXPECTED: "^
                                                  v^"\n"^
                                                  "FOUND: NONE\n")
                              | (SOME v1,SOME v2) =>
                                ("EXPECTED: "^
                                 v1^
                                 "\nACTUAL: "^
                                 v2^"\n")
                              | _ => "Uh, things worked fine?");
               in
                 if expected <> actual
                 then (print ("Uhoh, "^
                             (Metadata.dbg metadata)^
                             "\n"))
                 else ();
                 Assert.eq expected actual msg
               end);

val get_test1 =
  mk_get_test
      "get_test1"
      ["sml {file=example.sml}"
      ,"Trying to get the metadata for a present key"
      ,"should produce a (SOME value)"
      ,"for actual source code blocks"
      ]
      "file"
      (SOME "example.sml");

val get_test2 =
  mk_get_test
      "get_test2"
      ["sml* {file=example.sml}"
      ,"Trying to get the metadata for a present key"
      ,"should produce a (SOME value)"
      ,"for non-included ('example') source code blocks"
      ]
      "file"
      (SOME "example.sml");


val get_test3 =
  mk_get_test
      "get_test3"
      ["sml {file=example.sml}"
      ,"Trying to get the metadata for a missing key"
      ,"should produce a NONE result"
      ]
      "missing key"
      NONE;

val get_test4 =
  mk_get_test
      "get_test4"
      ["sml* {file=example.sml}"
      ,"Trying to get the metadata for a missing key"
      ,"should produce a NONE result"
      ]
      "missing key"
      NONE;

val get_test5 =
  mk_get_test
      "get_test5"
      [""
      ,"Three backticks have no metadata"
      ,"And in particular, an empty key-value store"
      ]
      "file"
      NONE;

val get_test6 =
  mk_get_test
      "get_test6"
      ["sml {file=\"my_file.sml\"}"
      ,"Values should be trimmed of delimiting quotation marks"
      ,"This is just some random"
      ,"Text captured between"
      ,"Three backticks"
      ]
      "file"
      (SOME "my_file.sml");

val get_test7 =
  mk_get_test
      "get_test7"
      ["sml {file='my_file.sml'}"
      ,"Values should be trimmed of delimiting single quotes"
      ,"This is just some random"
      ,"Text captured between"
      ,"Three backticks"
      ]
      "file"
      (SOME "my_file.sml");

val get_test8 =
  mk_get_test
      "get_test8"
      ["sml {\"file\"=my_file.sml}"
      ,"Keys should be trimmed of delimiting quotation marks"
      ,"This is just some random"
      ,"Text captured between"
      ,"Three backticks"
      ]
      "file"
      (SOME "my_file.sml");

val get_test9 =
  mk_get_test
      "get_test9"
      ["sml {'file'=my_file.sml}"
      ,"Keys should be trimmed of single quotes"
      ,"This is just some random"
      ,"Text captured between"
      ,"Three backticks"
      ]
      "file"
      (SOME "my_file.sml");

val get_test10 =
  mk_get_test
      "get_test10"
      ["sml {key1=val1,example, key2=val2}"
      ,"Keys without values are treated as if the value equals the key"
      ,"This is just some random"
      ,"Text captured between"
      ,"Three backticks"
      ]
      "example"
      (SOME "example");

val get_test11 =
  mk_get_test
      "get_test11"
      ["sml {key1=val1,'example', key2=val2}"
      ,"Keys without values are treated as if the value equals the key"
      ,"This should strip away single quote delimiters"
      ,"...I hope..."
      ]
      "example"
      (SOME "example");

val get_test12 =
  mk_get_test
      "get_test12"
      ["sml {key1=val1, \"example\", key2=val2}"
      ,"Keys without values are treated as if the value equals the key"
      ,"This should strip away double quote delimiters"
      ,"...I hope..."
      ]
      "example"
      (SOME "example");

val get_test13 =
  mk_get_test
      "get_test13"
      ["sml {key1=val1,key2=val2,key3=val3}"
      ,"Keys without values are treated as if the value equals the key"
      ,"This should strip away double quote delimiters"
      ,"...I hope..."
      ]
      "key2"
      (SOME "val2");

val get_test14 =
  mk_get_test
      "get_test14"
      ["sml {key1 = val1,key2 =val2,key3= val3}"
      ,"Keys without values are treated as if the value equals the key"
      ,"This should strip away double quote delimiters"
      ,"...I hope..."
      ]
      "key1"
      (SOME "val1");

val get_test15 =
  mk_get_test
      "get_test15"
      ["sml {key1 = val1,key2 =val2,key3= val3}"
      ,"Keys without values are treated as if the value equals the key"
      ,"This should strip away double quote delimiters"
      ,"...I hope..."
      ]
      "key2"
      (SOME "val2");

val get_test16 =
  mk_get_test
      "get_test16"
      ["sml {key1 = val1,key2 =val2,key3= val3}"
      ,"Keys without values are treated as if the value equals the key"
      ,"This should strip away double quote delimiters"
      ,"...I hope..."
      ]
      "key3"
      (SOME "val3");

val get_test17 =
  mk_get_test
      "get_test17"
      ["sml {key1 = val1, key2 =val2 , key3= val3}"
      ,"Keys without values are treated as if the value equals the key"
      ,"This should strip away double quote delimiters"
      ,"...I hope..."
      ]
      "key2"
      (SOME "val2");

val get_test18 =
  mk_get_test
      "get_test18"
      ["sml {key1 = val1, key2 =val2 ,",
       " key3= val3}"
      ,"Keys without values are treated as if the value equals the key"
      ,"This should strip away double quote delimiters"
      ,"...I hope..."
      ]
      "key3"
      (SOME "val3");

(* TODO: unit test `Metadata.same_name` *)

fun mk_same_name_test name expected src1 src2 =
  Test.new name
           (fn () =>
               let
                 val block1 = Substring.full
                                  (String.concatWith "\n" src1);
                 val block2 = Substring.full
                                  (String.concatWith "\n" src2);
                 val (metadata1,_) =
                   Metadata.from_codefence_block block1;
                 val (metadata2,_) =
                   Metadata.from_codefence_block block2;
                 val actual = Metadata.same_name metadata1 metadata2;
                 val msg = ("EXPECTED: "^
                            (if expected then "same"
                             else "different")^
                            " name, FOUND\nkvs1="^
                            (Metadata.dbg metadata1)^
                            "\nkvs2="^
                            (Metadata.dbg metadata2)^
                            "\n");
               in
                 Assert.eq expected actual msg
               end);

val same_name_test1 =
  mk_same_name_test
      "same_name_test1"
      true
      [ "sml {file = chunk.sig, important}"
      , "random source code here"
      , "very important"]
      [ "sml {file = chunk.sig}"
      , "equally random source code here"
      , "except not very important"];

val same_name_test2 =
  mk_same_name_test
      "same_name_test2"
      true
      [ "sml {key1 = val1, file = chunk.sml, important}"
      , "random source code here"
      , "very important"]
      [ "sml {file = chunk.sml, key2 = val2}"
      , "equally random source code here"
      , "except not very important"];

val same_name_test3 =
  mk_same_name_test
      "same_name_test3"
      false
      [ "sml {key1 = val1, file = chunk.sml, important}"
      , "random source code here"
      , "very important"]
      [ "sml {file = chunk.sig, key2 = val2}"
      , "equally random source code here"
      , "except not very important"];

val same_name_test4 =
  mk_same_name_test
      "same_name_test4"
      false
      [ "sml {key1 = val1, file = chunk.sml, important}"
      , "random source code here"
      , "very important"]
      [ "sml {name = 'first step', file = chunk.sml, key2 = val2}"
      , "equally random source code here"
      , "except not very important"];

val same_name_test5 =
  mk_same_name_test
      "same_name_test5"
      false
      [ "sml {key1 = val1, file = chunk.sml, name = important}"
      , "same file"
      , "very important"]
      [ "sml {name = 'first step', file = chunk.sml, key2 = val2}"
      , "same file but different name"
      , "should fail"];

val same_name_test6 =
  mk_same_name_test
      "same_name_test6"
      false
      [ "sml {key1 = val1, file = chunk.sig, name = important}"
      , "different file"
      , "very important"]
      [ "sml {name = important, file = chunk.sml, key2 = val2}"
      , "same name but different file"
      , "should fail"];

Test.register_suite "metadata_test" [
  extract_test1
, extract_test2
, extract_test3
, language_test1
, language_test2
, is_example_test1
, is_example_test2
, is_example_test3
, get_test1
, get_test2
, get_test3
, get_test4
, get_test5
, get_test6
, get_test7
, get_test8
, get_test9
, get_test10
, get_test11
, get_test12
, get_test13
, get_test14
, get_test15
, get_test16
, get_test17
, get_test18
, same_name_test1
, same_name_test2
, same_name_test3
, same_name_test4
, same_name_test5
, same_name_test6
];
                 
