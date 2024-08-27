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
                 Assert.!! "EXPECTED true FOUND false"
                        (EQUAL = Substring.compare(block, s))
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
                 Assert.!! ("## EXPECTED: \""^
                            (Substring.string expected)^
                            "\"\n## FOUND: "^
                            (Substring.string actual)^"\n")
                        (EQUAL = Substring.compare(expected, actual))
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
                 Assert.!! "EXPECTED exception thrown due to runaway metadata"
                        false
               end
               handle Metadata.Runaway msg =>
                      (Assert.!! "FOUND exception thrown due to runaway metadata"
                              true)
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
                 Assert.!! ("EXPECTED "^expected^"\nFOUND "^actual^"\n")
                        (expected = actual)
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
                 Assert.!! ("EXPECTED "^expected^"\nFOUND "^actual^"\n")
                        (expected = actual)
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
                 Assert.!! ("EXPECTED is_example = true\nFOUND ...=false\n")
                        (Metadata.is_example metadata)
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
                 Assert.!! ("EXPECTED is_example = false\nFOUND ...=true\n")
                        (not (Metadata.is_example metadata))
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
                 Assert.!! ("EXPECTED is_example = true\nFOUND ...=false\n")
                        (Metadata.is_example metadata)
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
                 Assert.!! msg (expected = actual)
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

Test.register_suite "test" [
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
];
                 
