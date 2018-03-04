;;; test-helper.el --- Helpers for gotest-ui-test.el

(load-file "./gotest-ui.el")
(require 's)
(require 'cl)

(cl-defmacro gotest-ui-test ((stdout-marker) &rest body)
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (with-temp-buffer
       (gotest-ui--clear-buffer (current-buffer))
       (gotest-ui-mode)
       (gotest-ui--setup-buffer (current-buffer) "(testing) go test -json . in /" '("go" "test" "-json" ".") "/")
       (let ((,stdout-marker (gotest-ui-test-stdout-marker)))
         ,@body))))

(def-edebug-spec gotest-ui-test ((&rest symbolp) body))

(defun gotest-ui-test-stdout-marker ()
  (with-current-buffer gotest-ui--process-buffer
    (point-min-marker)))

(defun gotest-ui-test-stderr-marker ()
  (with-current-buffer gotest-ui--stderr-process-buffer
    (point-min-marker)))

(defun gotest-ui-test-stdout-input (marker &rest json-lines)
  (let* ((input (s-join "\n" json-lines))
         (ui-buffer (current-buffer))
         (process-buffer gotest-ui--process-buffer))
    (gotest-ui-read-json process-buffer marker input)))

;;; test-helper.el ends here
