val metadata_extract_test1 =
  Test.new "metadata_extract_test1"
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

val metadata_extract_test2 =
  Test.new "metadata_extract_test2"
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
                 Assert.!! "EXPECTED true FOUND false"
                        (EQUAL = Substring.compare(expected, actual))
               end);

val metadata_language_test1 =
  Test.new "metadata_language_test1"
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

val metadata_language_test2 =
  Test.new "metadata_language_test2"
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

val metadata_is_example_test1 =
  Test.new "metadata_language_test1"
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

val metadata_is_example_test2 =
  Test.new "metadata_language_test2"
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

val metadata_is_example_test3 =
  Test.new "metadata_language_test3"
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

val metadata_get_test1 =
  Test.new "metadata_get_test1"
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
                 val expected = "example.sml";
                 val actual = Option.getOpt(Metadata.get metadata "file",
                                            "**wrong**" ^
                                            (Metadata.dbg metadata));
               in
                 Assert.!! ("EXPECTED "^expected ^
                            "\nFOUND "^actual ^ "\n")
                        (expected = actual)
               end);

val metadata_get_test2 =
  Test.new "metadata_get_test2"
           (fn () =>
               let
                 val block = Substring.full (
                     String.concatWith "\n"
                                       ["sml* {file=example.sml}"
                                       ,"Example blocks can have metadata!"
                                       ,"This is just some random"
                                       ,"Text captured between"
                                       ,"Three backticks"
                   ]);
                 val (metadata,_) =
                   Metadata.from_codefence_block block;
                 val expected = "example.sml";
                 val actual = Option.getOpt(Metadata.get metadata "file",
                                            "**wrong**" ^
                                            (Metadata.dbg metadata));
               in
                 Assert.!! ("EXPECTED "^expected ^
                            "\nFOUND "^actual ^ "\n")
                        (expected = actual)
               end);

val metadata_get_test3 =
  Test.new "metadata_get_test3"
           (fn () =>
               let
                 val block = Substring.full (
                     String.concatWith "\n"
                                       ["sml {file=example.sml}"
                                       ,"Example blocks can have metadata!"
                                       ,"This is just some random"
                                       ,"Text captured between"
                                       ,"Three backticks"
                   ]);
                 val (metadata,_) =
                   Metadata.from_codefence_block block;
                 val expected = NONE;
                 val actual = Metadata.get metadata "missing key";
               in
                 Assert.!! ("EXPECTED NONE" ^
                            "\nFOUND " ^
                            Option.getOpt(actual,"") ^ "\n")
                        (expected = actual)
               end);

val metadata_get_test4 =
  Test.new "metadata_get_test4"
           (fn () =>
               let
                 val block = Substring.full (
                     String.concatWith "\n"
                                       ["sml* {file=example.sml}"
                                       ,"Example blocks can have metadata!"
                                       ,"This is just some random"
                                       ,"Text captured between"
                                       ,"Three backticks"
                   ]);
                 val (metadata,_) =
                   Metadata.from_codefence_block block;
                 val expected = NONE;
                 val actual = Metadata.get metadata "missing key";
               in
                 Assert.!! ("EXPECTED NONE" ^
                            "\nFOUND " ^
                            Option.getOpt(actual,"") ^ "\n")
                        (expected = actual)
               end);

val metadata_get_test5 =
  Test.new "metadata_get_test5"
           (fn () =>
               let
                 val block = Substring.full (
                     String.concatWith "\n"
                                       [""
                                       ,"Example blocks can have metadata!"
                                       ,"This is just some random"
                                       ,"Text captured between"
                                       ,"Three backticks"
                   ]);
                 val (metadata,_) =
                   Metadata.from_codefence_block block;
                 val expected = NONE;
                 val actual = Metadata.get metadata "missing key";
               in
                 Assert.!! ("EXPECTED NONE" ^
                            "\nFOUND " ^
                            Option.getOpt(actual,"") ^ "\n")
                        (expected = actual)
               end);

Test.register_suite "metadata_test" [
  metadata_extract_test1
, metadata_extract_test2
, metadata_language_test1
, metadata_language_test2
, metadata_is_example_test1
, metadata_is_example_test2
, metadata_is_example_test3
, metadata_get_test1
, metadata_get_test2
, metadata_get_test3
, metadata_get_test4
, metadata_get_test5
];
                 
