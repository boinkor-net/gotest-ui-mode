;;; gotest-ui-test.el --- Tests for gotest-ui

(ert-deftest test/constructor ()
  (gotest-ui-test
   (let ((test (gotest-ui--make-test :name "TestFoobar"
                                     :package "github.com/antifuchs/testing"
                                     :status "running")))
     (should (equal (gotest-ui-thing-name test) "TestFoobar"))
     (should (equal (gotest-ui-test-package test) "github.com/antifuchs/testing"))
     (should (equal (gotest-ui-thing-status test) "running")))))

(ert-deftest test/pp ()
  (gotest-ui-test
   (let ((test (gotest-ui--make-test :name "TestFoobar"
                                     :package "github.com/antifuchs/testing"
                                     :status "running")))
     (should (equal (buffer-substring (point-min) (point-max))
                    "testing in ~/Hacks/gotest-ui/\nrunning github.com/antifuchs/testing.TestFoobar\n\n
Uses keymap ‘gotest-ui-mode-map’, which is not currently defined.\n\n"))

     ;; No extra output when the node is not expanded:
     (setf (gotest-ui-thing-output test) '("there\n" "hi\n"))
     (ewoc-refresh gotest-ui--ewoc)
     (should (equal (buffer-substring (point-min) (point-max))
                    "testing in ~/Hacks/gotest-ui/\nrunning github.com/antifuchs/testing.TestFoobar\n\n
Uses keymap ‘gotest-ui-mode-map’, which is not currently defined.\n\n"))

     ;; Expansion should print the output:
     (setf (gotest-ui-thing-expanded-p test) t)
     (ewoc-refresh gotest-ui--ewoc)
     (should (equal (buffer-substring (point-min) (point-max))
                    "testing in ~/Hacks/gotest-ui/\nrunning github.com/antifuchs/testing.TestFoobar\n\thi\n\tthere\n\n\n
Uses keymap ‘gotest-ui-mode-map’, which is not currently defined.\n\n")))))

;;; gotest-ui-test.el ends here
