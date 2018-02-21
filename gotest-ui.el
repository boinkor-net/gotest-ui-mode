(eval-when-compile
  (require 'cl))

(require 'ewoc)
(require 'json)
(require 'compile)

;;;; Data model:
;;;
;;; `gotest-ui-data' is the top-level structure, holding all the
;;; test/benchmark results for the current run.
(defstruct (gotest-ui-data (:constructor gotest-ui---make-data)
                           (:type vector))
  (running)
  (pass)
  (fail))

;;; `gotest-ui-thing' is a thing that can be under test: a
;;; package, or a single test.

(defstruct gotest-ui-thing
  (name)
  (ewoc)
  (expanded-p)
  (status)    ; 'pass or 'fail
  (output)    ; list of \n-terminated strings written as output
  (elapsed)   ; a floating-point amount of seconds
  )

;;; `gotest-ui-package' is a single test. It contains a status and
;;; output.
(defstruct (gotest-ui-test (:include gotest-ui-thing)
                           (:constructor gotest-ui--make-test-1))
  (package))

(defstruct (gotest-ui-status (:constructor gotest-ui--make-status-1))
  (state)
  (cmdline)
  (dir)
  (output)
  (node))

(cl-defun gotest-ui--make-status (ewoc cmdline dir)
  (let ((status (apply #'gotest-ui--make-status-1 :state "run" :cmdline cmdline :dir dir)))
    (let ((node (ewoc-enter-first ewoc status)))
      (setf (gotest-ui-status-node status) node))
    status))

(cl-defun gotest-ui--make-test (ewoc &rest args &key status package name &allow-other-keys)
  (let ((test (apply #'gotest-ui--make-test-1 :status (or status "run") args)))
    (let ((node (ewoc-enter-last ewoc test)))
      (setf (gethash test gotest-ui--nodes) node))
    test))

;;; Data manipulation routines:

(defun gotest-ui-ensure-test (ewoc package-name base-name)
  (let* ((test-name (format "%s.%s" package-name base-name))
         (test (gethash test-name gotest-ui--tests)))
    (if test
        test
      (setf (gethash test-name gotest-ui--tests)
            (gotest-ui--make-test ewoc :name base-name :package package-name)))))

(defun gotest-ui-update-status (new-state)
  (setf (gotest-ui-status-state gotest-ui--status) new-state)
  (ewoc-invalidate gotest-ui--ewoc (gotest-ui-status-node gotest-ui--status)))

(defun gotest-ui-update-status-output (new-output)
  (setf (gotest-ui-status-output gotest-ui--status) new-output)
  (ewoc-invalidate gotest-ui--ewoc (gotest-ui-status-node gotest-ui--status)))

;;;; Mode definition

(defvar gotest-ui-mode-map
  (let ((m (make-sparse-keymap)))
    (suppress-keymap m)
    ;; key bindings go here
    (define-key m (kbd "TAB") 'gotest-ui-toggle-expanded)
    (define-key m (kbd "g") 'gotest-ui-rerun)
    m))

(define-derived-mode gotest-ui-mode special-mode "go test UI"
  "Major mode for running go test with JSON output."
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory default-directory)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky))


(defun gotest-ui--clear-buffer (buffer)
  (let ((dir default-directory))
    (with-current-buffer buffer
      (when (buffer-live-p gotest-ui--process-buffer)
        (kill-buffer gotest-ui--process-buffer))
      (kill-all-local-variables)
      (let  ((buffer-read-only nil))
        (erase-buffer))
      (buffer-disable-undo)
      (setq-local default-directory dir))))

(defun gotest-ui--setup-buffer (buffer cmdline dir)
  (setq-local default-directory dir)
  (setq gotest-ui--cmdline cmdline
        gotest-ui--dir dir)
  (let ((ewoc (ewoc-create 'gotest-ui--pp-test nil nil t))
        (tests (make-hash-table :test #'equal))
        (nodes (make-hash-table :test #'eql)))
    (setq gotest-ui--tests tests)
    (setq gotest-ui--ewoc ewoc)
    (setq gotest-ui--nodes nodes)
    ;; Drop in the first few ewoc nodes:
    (setq gotest-ui--status (gotest-ui--make-status ewoc cmdline dir))
    ))

;;;; Commands:

(defun gotest-ui-toggle-expanded ()
  "Toggle expandedness of a test/package node"
  (interactive)
  (let* ((node (ewoc-locate gotest-ui--ewoc (point)))
         (data (ewoc-data node)))
    ;; (unless (or (null data) (not (gotest-ui-thing-p data)))
    ;;   (message "Not expandable."))
    (when (and data (gotest-ui-thing-p data))
      (setf (gotest-ui-thing-expanded-p data)
            (not (gotest-ui-thing-expanded-p data)))
      (ewoc-invalidate gotest-ui--ewoc node))))

(defun gotest-ui-rerun ()
  (interactive)
  (gotest-ui gotest-ui--cmdline :dir gotest-ui--dir))

;;;; Displaying the data:

(defvar-local gotest-ui--tests nil)
(defvar-local gotest-ui--ewoc nil)
(defvar-local gotest-ui--nodes nil)
(defvar-local gotest-ui--status nil)
(defvar-local gotest-ui--process-buffer nil)
(defvar-local gotest-ui--ui-buffer nil)
(defvar-local gotest-ui--process nil)
(defvar-local gotest-ui--cmdline nil)
(defvar-local gotest-ui--dir nil)

(cl-defun gotest-ui (cmdline &key dir)
  (interactive "sgo test -json ./...")
  (let* ((name (format "*go test: %s in %s" cmdline dir))
         (buffer (get-buffer-create name)))
    (unless (eql buffer (current-buffer))
      (switch-to-buffer-other-window buffer))
    (with-current-buffer buffer
      (let ((default-directory dir))
        (gotest-ui--clear-buffer buffer)
        (gotest-ui-mode)
        (gotest-ui--setup-buffer buffer cmdline dir))
      (setq gotest-ui--process-buffer (generate-new-buffer (format " *gotest-ui: %s in %s" cmdline dir)))
      (with-current-buffer gotest-ui--process-buffer
        (setq gotest-ui--ui-buffer buffer))
      (setq gotest-ui--process
            (start-process-shell-command name gotest-ui--process-buffer cmdline))
      (set-process-filter gotest-ui--process #'gotest-ui-read-json)
      (set-process-sentinel gotest-ui--process #'gotest-ui--process-sentinel))))

(defun gotest-ui-pp (elt)
  ;; TODO
  )

(defun gotest-ui--pp-test (test)
  (cond
   ((gotest-ui-status-p test)
    (insert (format "%s %s in %s\n\n"
                    (gotest-ui-status-state test)
                    (gotest-ui-status-cmdline test)
                    (gotest-ui-status-dir test)))
    (unless (zerop (length (gotest-ui-status-output test)))
      (insert (format "\n\n%s" (gotest-ui-status-output test)))))
   ((gotest-ui-test-p test)
    (let ((status (gotest-ui-thing-status test))
          (package (gotest-ui-test-package test))
          (name (gotest-ui-thing-name test)))
      (insert (propertize (format "%s" status)
                          :face (case status
                                  (fail 'error)
                                  (otherwise 'info))))
      (insert (format " %s.%s" package name))
      (when-let ((elapsed (gotest-ui-thing-elapsed test)))
        (insert (format " (%fs)" elapsed))))
    (when (gotest-ui-thing-expanded-p test)
      (insert "\n")
      (dolist (line (reverse (gotest-ui-thing-output test)))
        (insert "\t")
        (insert line)))
    (insert "\n"))
   ))

;;;; Handling input:

(defun gotest-ui--process-sentinel (proc event)
  (let* ((process-buffer (process-buffer proc))
         (ui-buffer (with-current-buffer process-buffer gotest-ui--ui-buffer))
         (inhibit-quit t))
    (with-local-quit
      (with-current-buffer ui-buffer
        (cond
         ((string= event "finished\n")
          (gotest-ui-update-status 'pass))
         ((s-prefix-p "exited abnormally" event)
          (gotest-ui-update-status 'fail))
         (t
          (gotest-ui-update-status event)))))))

(defun gotest-ui-read-json (proc input)
  (let* ((process-buffer (process-buffer proc))
         (ui-buffer (with-current-buffer process-buffer gotest-ui--ui-buffer))
         (inhibit-quit t))
    (with-local-quit
      (when (buffer-live-p process-buffer)
        (with-current-buffer process-buffer
          (cond
           ((= (point-min) (point-max))
            ;; Buffer is empty, decide whether to treat this as JSON
            ;; or as compiler spew:
            (if (= (string-to-char input) ?\{)
                (gotest-ui-read-json-1 proc process-buffer ui-buffer input)
              (gotest-ui-read-compiler-spew proc process-buffer ui-buffer input)))
           ((= (char-after (point-min)) ?\{)
            ;; We have read JSON, let's continue reading JSON.
            (gotest-ui-read-json-1 proc process-buffer ui-buffer input))
           (t
            ;; Started out as compiler spew, let's continue reading compiler spew.
            (gotest-ui-read-compiler-spew proc process-buffer ui-buffer input))))))))

(defun gotest-ui-read-compiler-spew (proc process-buffer ui-buffer input)
  (with-current-buffer process-buffer
    (save-excursion
      (message "existing compiler spew detected!")
      (goto-char (point-max))
      (insert input)
      (let ((all-output (buffer-string)))
        (with-current-buffer ui-buffer
          (gotest-ui-update-status-output all-output))))))

(defun gotest-ui-read-json-1 (proc process-buffer ui-buffer input)
  (with-current-buffer process-buffer
    (save-excursion
      ;; insert the chunk of output at the end
      (goto-char (point-max))
      (insert input)

      ;; try to read the next object (which is hopefully complete now):
      (let ((last-object-start (process-mark proc)))
        (goto-char last-object-start)
        (cl-loop
         (condition-case err
             (let ((obj (json-read)))
               (set-marker (process-mark proc) (point))
               (with-current-buffer ui-buffer
                 (gotest-ui-update-test-status obj)))
           (json-error (return))
           (wrong-type-argument
            (if (and (eql (cadr err) 'characterp)
                     (eql (caddr err) :json-eof))
                ;; This is peaceful & we can ignore it:
                (return)
              (signal 'wrong-type-argument err)))))))))

(defun gotest-ui-update-test-status (json)
  (let-alist json
    (let ((action (intern .Action))
          test)
      (case action
        (run (setq test (gotest-ui-ensure-test gotest-ui--ewoc .Package .Test)))
        (output (setq test (gotest-ui-ensure-test gotest-ui--ewoc .Package .Test))
                (push .Output (gotest-ui-thing-output test)))
        (pass
         (setq test (gotest-ui-ensure-test gotest-ui--ewoc .Package .Test))
         (setf (gotest-ui-thing-status test) 'pass
               (gotest-ui-thing-elapsed test) .Elapsed))
        (fail
         (setq test (gotest-ui-ensure-test  gotest-ui--ewoc .Package .Test))
         (setf (gotest-ui-thing-status test) 'fail
               (gotest-ui-thing-elapsed test) .Elapsed))
        (skip
         (setq test (gotest-ui-ensure-test  gotest-ui--ewoc .Package .Test))
         (setf (gotest-ui-thing-status test) 'skip
               (gotest-ui-thing-elapsed test) .Elapsed)))
      (when test
        (ewoc-invalidate gotest-ui--ewoc (gethash test gotest-ui--nodes))))))

;;;; Commands for go-mode:

(defun gotest-ui-current-file ()
  "Launch go test on the current buffer file."
  (interactive)
  (let ((data (go-test--get-current-file-testing-data)))
    (gotest-ui (s-concat "go test -json " "-run='" data "' ."))))

(defun gotest-ui-current-project ()
  "Launch go test on the current buffer's project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (gotest-ui "go test -json  ./...")))
