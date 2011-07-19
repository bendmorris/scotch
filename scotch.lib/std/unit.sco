tests = []

assert_equal(a, b) = if a == b then [[n, "pass"]] else [[n, (str(a) + " != " + str(b))]]
                     where n := len(tests) + 1

run_tests(tests) =
  do print tests
     print(if [for test in tests, test @ 1] == (["pass"] * len(tests))
           then "All " str(len(tests)) " tests passed."
           else str([for test in tests, test, test @ 1 != "pass"]) + "\nSome tests failed.")
     print("Done!")
