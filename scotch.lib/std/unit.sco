tests = []

assert_equal(a, b) = [if a == b then "pass" else a]

run_tests =
  do if all([for test in tests, if test == "pass" then true else false])
     then print "All " + len(tests) + " tests passed."
     else do print tests
             print "Some tests failed."
             
     print "Done!"
