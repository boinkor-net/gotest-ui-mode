;;; test-helper.el --- Helpers for gotest-ui-test.el

(load-file "./gotest-ui.el")

(defmacro gotest-ui-test (&rest body)
  `(save-window-excursion
     (with-temp-buffer
       (gotest-ui--setup-buffer (current-buffer) "testing")
       ,@body)))

;;; test-helper.el ends here
