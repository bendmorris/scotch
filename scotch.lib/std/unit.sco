tests = []

assert_equal(a, b) = [if a == b then "pass" else (a + " != " + b)]

run_tests =
  do print if all([for test in tests, if test == "pass" then true else false])
           then "All " + len(tests) + " tests passed."
           else show(tests) + "\nSome tests failed."
             
     print "Done!"
