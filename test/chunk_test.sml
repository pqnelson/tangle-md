fun mk_same_test_factory pred printed_key =
  fn name => fn lhs => fn rhs => fn expected =>
     (Test.new
          name
          (fn () =>
               let
                 val left_raw = String.concatWith "\n" lhs;
                 val left = (Chunk.from o
                             Metadata.from_codefence_block o
                             Substring.full) left_raw;
                 val right_raw = String.concatWith "\n" rhs;
                 val right = (Chunk.from o
                              Metadata.from_codefence_block o
                              Substring.full) right_raw;
                 val msg = concat ["## EXPECTED lhs = ```"
                                  , left_raw
                                  , "\n``` "
                                  , "to "
                                  , if expected then "" else "not"
                                  ," have same name as rhs = ```"
                                  , right_raw
                                  , "\n``` \n"
                                  , "## ACTUAL lhs."
                                  , printed_key
                                  , if expected then "<>" else "="
                                  , "rhs."
                                  , printed_key
                                  , "\n"];
               in
                 Assert.!! msg
                        (expected = (pred left right))
               end));

val mk_same_name_test =
  mk_same_test_factory Chunk.same_name "name";
(*
fun mk_same_name_test name lhs rhs expected =
  Test.new name
           (fn () =>
               let
                 val left_raw = String.concatWith "\n" lhs;
                 val left = (Chunk.from o
                             Metadata.from_codefence_block o
                             Substring.full) left_raw;
                 val right_raw = String.concatWith "\n" rhs;
                 val right = (Chunk.from o
                              Metadata.from_codefence_block o
                              Substring.full) right_raw;
                 val msg = concat ["## EXPECTED lhs = ```"
                                  , left_raw
                                  , "\n``` "
                                  , "to "
                                  , if expected then "" else "not"
                                  ," have same name as rhs = ```"
                                  , right_raw
                                  , "\n``` \n"
                                  , "## ACTUAL lhs.name "
                                  , if expected then "<>" else "="
                                  , "rhs.name\n"];
               in
                 Assert.!! msg
                        (expected = (Chunk.same_name left right))
               end);
                       *)

val same_name_test1 =
  mk_same_name_test "test1"
                    ["md"
                    ,"Random stuff"
                    ,"in a Markdown example"
                    ]
                    ["sml"
                    ,"fun make_test name = "
                    ,"    print name;"
                    ]
                    true;

val same_name_test2 =
  mk_same_name_test "same_name_test2"
                    ["md {file=example.md}"
                    ,"Random stuff"
                    ,"in a Markdown example"
                    ]
                    ["sml"
                    ,"fun make_test name = "
                    ,"    print name;"
                    ]
                    false;

val same_name_test3 =
  mk_same_name_test "same_name_test3"
                    ["md {file=example.md}"
                    ,"Random stuff"
                    ,"in a Markdown example"
                    ]
                    ["sml {file=test_runner.sml}"
                    ,"fun make_test name = "
                    ,"    print name;"
                    ]
                    false;

val same_name_test4 =
  mk_same_name_test "same_name_test4"
                    ["md"
                    ,"Random stuff"
                    ,"in a Markdown example"
                    ]
                    ["sml {file=test_runner.sml}"
                    ,"fun make_test name = "
                    ,"    print name;"
                    ]
                    false;

val same_name_test5 =
  mk_same_name_test "same_name_test5"
                    ["md {file=example.md}"
                    ,"Random stuff"
                    ,"in a Markdown example"
                    ]
                    ["md {file=example.md}"
                    ,"Block code looks like:"
                    ,"  ```sml"
                    ,"  fun make_test name = "
                    ,"      print name;"
                    ,"  ```"
                    ,"And eventually will be pretty printed."
                    ]
                    true;

val same_name_test6 =
  mk_same_name_test "same_name_test6"
                    ["md {file=example.md, name=Header for Markdown}"
                    ,"---"
                    ,"title: Random stuff"
                    ,"---"
                    ]
                    ["md {file=example.md}"
                    ,"Block code looks like:"
                    ,"  ```sml"
                    ,"  fun make_test name = "
                    ,"      print name;"
                    ,"  ```"
                    ,"And eventually will be pretty printed."
                    ]
                    false;

val same_name_test7 =
  mk_same_name_test "same_name_test7"
                    ["md {name=Header for Markdown}"
                    ,"---"
                    ,"title: Random stuff"
                    ,"---"
                    ]
                    ["md {name=Header for Markdown}"
                    ,"This example SHOULD be true"
                    ,"exemplifying the fact that "
                    ,"sharing the same name AND "
                    ,"missing the file SHOULD"
                    ,"satisfy the condition of 'having the same name'"
                    ]
                    true;

val same_name_test8 =
  mk_same_name_test "same_name_test8"
                    ["md {name=Header for Markdown}"
                    ,"---"
                    ,"title: Random stuff"
                    ,"---"
                    ]
                    ["sml {name=Header for Markdown}"
                    ,"This example SHOULD be true"
                    ,"exemplifying the fact that "
                    ,"sharing the same name AND "
                    ,"having different languages AND"
                    ,"missing the file SHOULD"
                    ,"satisfy the condition of 'having the same name'"
                    ]
                    true;

(* * Testing the predicate if two chunks have the same
programming lanuage *)
val mk_same_language_test =
  mk_same_test_factory Chunk.same_language "language";

val same_language_test1 =
  mk_same_language_test
      "same_language_test1"
      ["md {name=Header for Markdown}"
      ,"---"
      ,"title: Random stuff"
      ,"---"
      ]
      ["sml {name=Header for Markdown}"
      ,"This example SHOULD be true"
      ,"exemplifying the fact that "
      ,"sharing the same name AND "
      ,"having different languages AND"
      ,"missing the file SHOULD"
      ,"satisfy the condition of 'having the same name'"
      ]
      false;

val same_language_test2 =
  mk_same_language_test
      "same_language_test2"
      ["sml {name=Header for Markdown}"
      ,"---"
      ,"title: Random stuff"
      ,"---"
      ]
      ["sml {name=Header for Markdown}"
      ,"This example SHOULD be true"
      ,"exemplifying the fact that "
      ,"sharing the same name AND "
      ,"having different languages AND"
      ,"missing the file SHOULD"
      ,"satisfy the condition of 'having the same name'"
      ]
      true;

val same_language_test3 =
  mk_same_language_test
      "same_language_test3"
      [""
      ,"---"
      ,"title: Random stuff"
      ,"---"
      ]
      [""
      ,"This example SHOULD be true"
      ,"exemplifying the fact that "
      ,"sharing the same name AND "
      ,"having different languages AND"
      ,"missing the file SHOULD"
      ,"satisfy the condition of 'having the same name'"
      ]
      false;

Test.register_suite "chunk_test" [
  same_name_test1
, same_name_test2
, same_name_test3
, same_name_test4
, same_name_test5
, same_name_test6
, same_name_test7
, same_name_test8
, same_language_test1
, same_language_test2
, same_language_test3
];
