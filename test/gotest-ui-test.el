;;; gotest-ui-test.el --- Tests for gotest-ui

(ert-deftest can-read-stdout ()
  (gotest-ui-test (stdout-marker)
    (gotest-ui-test-stdout-input
     stdout-marker
     "{\"Time\":\"2018-03-04T16:06:29.080756913+01:00\",\"Action\":\"run\",\"Package\":\"foo\",\"Test\":\"TestHi\"}"
     "{\"Time\":\"2018-03-04T16:06:29.083227527+01:00\",\"Action\":\"output\",\"Package\":\"foo\",\"Test\":\"TestHi\",\"Output\":\"=== RUN\tTestHi\\n\"}"
     "{\"Time\":\"2018-03-04T16:06:29.083278199+01:00\"")
    (gotest-ui-test-stdout-input
     stdout-marker
     ",\"Action\":\"pass\",\"Package\":\"foo\",\"Test\":\"TestHi\",\"Elapsed\":0.0001}"
     "")
    ))

(ert-deftest interrupted-test-output-and-invalidation ()
  (gotest-ui-test (stdout-marker)
    (gotest-ui-test-stdout-input
     stdout-marker
     "{\"Time\":\"2018-03-04T16:06:29.080756913+01:00\",\"Action\":\"run\",\"Package\":\"foo\",\"Test\":\"TestHi\"}"
     "{\"Time\":\"2018-03-04T16:06:29.083227527+01:00\",\"Action\":\"output\",\"Package\":\"foo\",\"Test\":\"TestHi\",\"Output\":\"=== RUN\tTestHi\\n\"}"
     "{\"Time\":\"2018-03-04T16:06:29.083227527+01:00\",\"Action\":\"output\",\"Package\":\"foo\",\"Test\":\"TestHi\",\"Output\":\"Yay!\n\"}"
     "{\"Time\":\"2018-03-04T16:06:29.083278199+01:00\",\"Action\":\"pass\",\"Package\":\"foo\",\"Test\":\"TestHi\",\"Elapsed\":0.0001}"
     "{\"Time\":\"2018-03-04T16:06:29.083227527+01:00\",\"Action\":\"run\",\"Package\":\"foo\",\"Test\":\"TestThere\"}"
     )))

(ert-deftest test-sorting ()
  (gotest-ui-test (stdout-marker)
    (labels ((pkg (name) (gotest-ui-ensure-test gotest-ui--ewoc name nil))
             (test (pkg name) (gotest-ui-ensure-test gotest-ui--ewoc pkg name)))
      ;; Packages sort lexicographically:
      (should (gotest-ui-test->= (pkg "foo2") (pkg "foo1") ))
      ;; Tests sort in after their packages:
      (should (gotest-ui-test->= (test "foo1" "TestFoo") (pkg "foo1")))
      ;; Tests in a package sort lexicographically:
      (should (gotest-ui-test->= (test "foo1" "TestFoo2") (test "foo1" "TestFoo1")))
      ;; Tests in different packages sort by package names:
      (should (gotest-ui-test->= (test "foo2" "TestFoo") (test "foo1" "TestFoo")))
      ;; Subtests sort under their parent
      (should (gotest-ui-test->= (test "foo1" "TestFoo/Subtest") (test "foo1" "TestFoo"))))))

;;; gotest-ui-test.el ends here
