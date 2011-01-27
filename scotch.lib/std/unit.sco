tests = []

assert_equal(a, b) = [if a == b then "pass" else (a + " != " + b)]

run_tests(tests) =
  do print if all([for test in tests, test == "pass"])
           then "All " + len(tests) + " tests passed."
           else show([for test in tests, test, test != "pass"]) + "\nSome tests failed."
             
     print "Done!"
