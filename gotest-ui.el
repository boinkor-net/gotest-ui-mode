;;; gotest-ui.el --- Major mode for running go test -json

;; Copyright 2018 Andreas Fuchs
;;           2013-2015 Samuel Tonini, Matt DeBoard, Andreas Fuchs
;; Authors: Andreas Fuchs <asf@boinkor.net>,

;; URL: https://github.com/antifuchs/gotest-ui-mode
;; Created: Feb 18, 2018
;; Keywords: languages go
;; Version: 0.1.0
;; Package-Requires: ((emacs "25") (json) (compile))

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;  Provides support for running go tests with a nice user interface
;;  that allows folding away output, highlighting failing tests.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ewoc)
(require 'json)
(require 'compile)

(defface gotest-ui-pass-face '((t :foreground "green"))
  "Face for displaying the status of a passing test."
  :group 'gotest-ui)

(defface gotest-ui-skip-face '((t :foreground "grey"))
  "Face for displaying the status of a skipped test."
  :group 'gotest-ui)

(defface gotest-ui-fail-face '((t :foreground "pink" :weight bold))
  "Face for displaying the status of a failed test."
  :group 'gotest-ui)

(defface gotest-ui-link-face '((t :foreground "white" :weight bold))
  "Face for displaying links to go source files."
  :group 'gotest-ui)

(defcustom gotest-ui-expand-test-statuses '(fail)
  "Statuses to expand test cases for.
Whenever a test enters this state, it is automatically expanded.")

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
  (status)
  (buffer)    ; the buffer containing this test's output
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
  (let ((status (gotest-ui--make-status-1 :state "run" :cmdline cmdline :dir dir)))
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

(defun gotest-ui-ensure-output-buffer (thing)
  (unless (gotest-ui-thing-buffer thing)
    (with-current-buffer
        (setf (gotest-ui-thing-buffer thing)
              (generate-new-buffer (format " *%s" (gotest-ui-thing-name thing))))
      (setq-local gotest-ui-parse-marker (point-min-marker))
      (setq-local gotest-ui-insertion-marker (point-min-marker))
      (set-marker-insertion-type gotest-ui-insertion-marker t)))
  (gotest-ui-thing-buffer thing))

(defun gotest-ui-mouse-open-file (event)
  "In gotest-ui mode, open the file/line reference in another window."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event)))
        file line)
    (if (not (windowp window))
        (error "No file chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (setq file (gotest-ui-get-file-for-visit))
      (setq line (gotest-ui-get-line-for-visit)))
    (unless (file-exists-p file)
      (error "Could not open %s:%d" file line))
    (with-current-buffer (find-file-other-window file)
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun gotest-ui-get-file-for-visit ()
  (get-text-property (point) 'gotest-ui-file))

(defun gotest-ui-get-line-for-visit ()
  (string-to-number (get-text-property (point) 'gotest-ui-line)))

(defun gotest-ui-file-from-gopath (package file-basename)
  (if (file-name-absolute-p file-basename)
      file-basename
    (let ((gopath (or (getenv "GOPATH")
                      (expand-file-name "~/go"))))
      (expand-file-name (concat gopath "/src/" package "/" file-basename)))))

(defvar gotest-ui-click-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'gotest-ui-mouse-open-file)
    map))

(defun gotest-ui-ensure-parsed (thing)
  (save-excursion
    (goto-char gotest-ui-parse-marker)
    (while (re-search-forward "\\([^ \t]+\\.go\\):\\([0-9]+\\)" gotest-ui-insertion-marker t)
      (let* ((file-basename (match-string 1))
             (file (gotest-ui-file-from-gopath (gotest-ui-test-package thing) file-basename)))
        (set-text-properties (match-beginning 0) (match-end 0)
                             `(face gotest-ui-link-face
                                    gotest-ui-file ,file
                                    gotest-ui-line ,(match-string 2)
                                    keymap ,gotest-ui-click-map
                                    follow-link t
                                    ))))
    (set-marker gotest-ui-parse-marker gotest-ui-insertion-marker)))

(defun gotest-ui-update-thing-output (thing output)
  (with-current-buffer (gotest-ui-ensure-output-buffer thing)
    (goto-char gotest-ui-insertion-marker)
    (let ((overwrites (split-string output "\r")))
      (insert (car overwrites))
      (dolist (segment (cdr overwrites))
        (let ((delete-to (point)))
          (forward-line 0)
          (delete-region (point) delete-to))
        (insert segment)))
    (set-marker gotest-ui-insertion-marker (point))
    (gotest-ui-ensure-parsed thing)))

;; TODO: clean up buffers on kill

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
  (let* ((dir (or dir default-directory))
         (name (format "*go test: %s in %s" cmdline dir))
         (buffer (get-buffer-create name)))
    (unless (eql buffer (current-buffer))
      (switch-to-buffer-other-window buffer))
    (with-current-buffer buffer
      (let ((default-directory dir))
        (gotest-ui--clear-buffer buffer)
        (gotest-ui-mode)
        (gotest-ui--setup-buffer buffer cmdline dir))
      (setq gotest-ui--process-buffer (generate-new-buffer (format " *%s" name)))
      (with-current-buffer gotest-ui--process-buffer
        (setq gotest-ui--ui-buffer buffer))
      (setq gotest-ui--process
            (start-process-shell-command name gotest-ui--process-buffer cmdline))
      (set-process-filter gotest-ui--process #'gotest-ui-read-json)
      (set-process-sentinel gotest-ui--process #'gotest-ui--process-sentinel))))

(defun gotest-ui-pp-status (status)
  (propertize (format "%s" status)
              'face
              (case status
                (fail 'gotest-ui-fail-face)
                (skip 'gotest-ui-skip-face)
                (pass 'gotest-ui-pass-face)
                (otherwise 'default))))

(defun gotest-ui--pp-test-output (test)
  (with-current-buffer (gotest-ui-ensure-output-buffer test)
    (propertize (buffer-substring (point-min) (point-max))
                'line-prefix "\t")))

(defun gotest-ui--pp-test (test)
  (cond
   ((gotest-ui-status-p test)
    (insert (gotest-ui-pp-status (gotest-ui-status-state test)))
    (insert (format " %s in %s\n\n"
                    (gotest-ui-status-cmdline test)
                    (gotest-ui-status-dir test)))
    (unless (zerop (length (gotest-ui-status-output test)))
      (insert (format "\n\n%s" (gotest-ui-status-output test)))))
   ((gotest-ui-test-p test)
    (let ((status (gotest-ui-thing-status test))
          (package (gotest-ui-test-package test))
          (name (gotest-ui-thing-name test)))
      (insert (gotest-ui-pp-status status))
      (insert " ")
      (insert (if name
                  (format "%s.%s" package name)
                package))
      (when-let ((elapsed (gotest-ui-thing-elapsed test)))
        (insert (format " (%.3fs)" elapsed))))
    (when (gotest-ui-thing-expanded-p test)
      (insert "\n")
      (insert (gotest-ui--pp-test-output test)))
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
        (let ((nodes
               (cl-loop
                for node = (condition-case err
                               (let ((obj (json-read)))
                                 (set-marker (process-mark proc) (point))
                                 (with-current-buffer ui-buffer
                                   (gotest-ui-update-test-status obj)))
                             (json-error (return nodes))
                             (wrong-type-argument
                              (if (and (eql (cadr err) 'characterp)
                                       (eql (caddr err) :json-eof))
                                  ;; This is peaceful & we can ignore it:
                                  (return nodes)
                                (signal 'wrong-type-argument err))))
                when node collect node into nodes)))
          (when nodes
            (with-current-buffer ui-buffer
              (apply #'ewoc-invalidate gotest-ui--ewoc (cl-remove-duplicates nodes)))))))))

(defun gotest-ui-maybe-expand (test)
  (when (memq (gotest-ui-test-status test) gotest-ui-expand-test-statuses)
    (setf (gotest-ui-test-expanded-p test) t)))

(defun gotest-ui-update-test-status (json)
  (let-alist json
    (let ((action (intern .Action))
          (test (gotest-ui-ensure-test gotest-ui--ewoc .Package .Test)))
      (case action
        (run t)
        (output (gotest-ui-update-thing-output test .Output))
        (pass
         (setf (gotest-ui-thing-status test) 'pass
               (gotest-ui-thing-elapsed test) .Elapsed)
         (gotest-ui-maybe-expand test))
        (fail
         (setf (gotest-ui-thing-status test) 'fail
               (gotest-ui-thing-elapsed test) .Elapsed)
         (gotest-ui-maybe-expand test))
        (skip
         (setf (gotest-ui-thing-status test) 'skip
               (gotest-ui-thing-elapsed test) .Elapsed)
         (gotest-ui-maybe-expand test))
        (otherwise
         (setq test nil)))
      (when test (gethash test gotest-ui--nodes)))))

;;;; Commands for go-mode:

;;;###autoload
(defun gotest-ui-current-test ()
  "Launch go test with the test that (point) is in."
  (interactive)
  (cl-destructuring-bind (test-suite test-name) (go-test--get-current-test-info)
    (let ((test-flag (if (> (length test-suite) 0) "-m " "-run "))
          (additional-arguments (if go-test-additional-arguments-function
                                    (funcall go-test-additional-arguments-function
                                             test-suite test-name) "")))
      (when test-name
        (gotest-ui (s-concat "go test -json " test-flag test-name additional-arguments "\\$ ."))))))

;;;###autoload
(defun gotest-ui-current-file ()
  "Launch go test on the current buffer file."
  (interactive)
  (let ((data (go-test--get-current-file-testing-data)))
    (gotest-ui (s-concat "go test -json " "-run='" data "' ."))))

;;;###autoload
(defun gotest-ui-current-project ()
  "Launch go test on the current buffer's project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (gotest-ui "go test -json  ./...")))

(provide 'gotest-ui)

;;; gotest-ui.el ends here
