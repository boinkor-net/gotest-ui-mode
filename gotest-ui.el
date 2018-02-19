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

;;; `gotest-ui-package' is a single package under test. It contains
;;; benchmarks and tests.

(defstruct (gotest-ui-test (:include gotest-ui-thing)
                           (:constructor gotest-ui--make-test-1))
  (package))

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

;;;; Mode definition

(defvar gotest-ui-mode-map
  (let ((m (make-sparse-keymap)))
    (suppress-keymap m)
    ;; key bindings go here
    (define-key m (kbd "TAB") 'gotest-ui-toggle-expanded)
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
  (let ((ewoc (ewoc-create 'gotest-ui--pp-test (format "%s in %s" cmdline default-directory)
                           (substitute-command-keys "\n\\{gotest-ui-mode-map}")))
        (tests (make-hash-table :test #'equal))
        (nodes (make-hash-table :test #'eql)))
    (setq gotest-ui--tests tests)
    (setq gotest-ui--ewoc ewoc)
    (setq gotest-ui--nodes nodes)))

;;;; Commands:

(defun gotest-ui-toggle-expanded ()
  "Toggle expandedness of a test/package node"
  (interactive)
  (let* ((node (ewoc-locate gotest-ui--ewoc (point)))
         (data (ewoc-data node)))
    ;; (unless (or (null data) (not (gotest-ui-thing-p data)))
    ;;   (message "Not expandable."))
    (setf (gotest-ui-thing-expanded-p data)
          (not (gotest-ui-thing-expanded-p data)))
    (ewoc-invalidate gotest-ui--ewoc node)))

;;;; Displaying the data:

(defvar-local gotest-ui--tests nil)
(defvar-local gotest-ui--ewoc nil)
(defvar-local gotest-ui--nodes nil)
(defvar-local gotest-ui--process-buffer nil)
(defvar-local gotest-ui--ui-buffer nil)
(defvar-local gotest-ui--process nil)
(defvar-local gotest-ui--cmdline nil)
(defvar-local gotest-ui--dir nil)

(cl-defun gotest-ui (cmdline &key dir)
  (interactive "sgo test -json ./...")
  (let* ((name (format "*go test: %s in %s" cmdline dir))
         (buffer (get-buffer-create name)))
    (switch-to-buffer-other-window buffer)
    (with-current-buffer buffer
      (let ((default-directory dir))      (gotest-ui--clear-buffer buffer)
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
      (insert line))))

;;;; Handling input:

(defun gotest-ui--process-sentinel (process event)
  ;; TODO: update ui buffer?
  )

(defun gotest-ui-read-json (proc input)
  (let* ((process-buffer (process-buffer proc))
         (ui-buffer (with-current-buffer process-buffer gotest-ui--ui-buffer))
         (inhibit-quit t))
    (with-local-quit
      (when (buffer-live-p process-buffer)
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
                    (signal 'wrong-type-argument err))))))))))))

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
