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

;;; gotest-ui-test.el ends here
