fun mk_tangle_test name (expected : string) (input : string list)
  : Test.t =
  Test.new name (fn () =>
                    let
                      val actual = form_src (concat input);
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

Test.register_suite "tangle_test" [
  tangle_test1
, tangle_test2
];
