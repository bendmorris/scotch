tests = []

assert_equal(a, b) = [if a == b then "pass" else a]

run_tests =
  do print tests
     print if all([for test in tests, if test == "pass" then true else false])
           then "All " + len(tests) + " tests passed."
           else "Some tests failed."
           
     print "Done!"
