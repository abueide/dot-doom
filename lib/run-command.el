;;; run-command.el --- Execute commands in term buffer -*- lexical-binding: t -*-
;;; Commentary:
;;; This package allows you to run a command asynchronously with term.
;;; It has options to show/hide the output console depending on the command's exit code.
;;; Code:
(require 'term)
(require 'subr-x)
(require 'evil)
(require 'cl-lib)
(require 'ert)  ; Emacs Lisp Testing Framework

(defvar run-command-debug t "Enable debug for run-command.")
(defvar run-command-buffer-policy "onSuccess"
  "Output Buffer Policy: always, never, onSuccess, onFailure.")

(defun run-command--log (message &rest args)
  "Log MESSAGE with ARGS if `run-command-debug` is enabled."
  (when run-command-debug
    (apply #'message (concat "[run-command]: " message) args)))

(defun run-command (command)
  "Run COMMAND in a term buffer."
  (run-command--log "Running: %s" command)
  (let* ((buffer-name "*run-command-output*")
         (buffer (get-buffer-create buffer-name)))
    (run-command--log "Creating term buffer: %s" buffer-name)
    (with-current-buffer buffer
      (term-mode)
      (term-exec buffer "run-command" "bash" nil (list "-l" "-i" "-c" command)))
    (display-buffer-at-bottom buffer '((window-height . 0.25)))
    (select-window (get-buffer-window buffer))
    (evil-normal-state)
    (set-process-sentinel (get-buffer-process buffer)
                          (lambda (proc _event)
                            (let ((exit-code (process-exit-status proc)))
                              (run-command--log "Exit code: %d" exit-code)
                              (cond
                               ((string= run-command-buffer-policy "always")
                                (run-command--log "Policy: always keep buffer."))
                               ((string= run-command-buffer-policy "never")
                                (run-command--log "Policy: never keep buffer.")
                                (delete-window (get-buffer-window buffer)))
                               ((and (string= run-command-buffer-policy "onSuccess")
                                     (= exit-code 0))
                                (run-command--log "Policy: onSuccess, closing buffer.")
                                (delete-window (get-buffer-window buffer)))
                               ((and (string= run-command-buffer-policy "onFailure")
                                     (/= exit-code 0))
                                (run-command--log "Policy: onFailure, closing buffer.")
                                (delete-window (get-buffer-window buffer)))))))))

(defun run-selected-command ()
  "Run the currently selected text as a command."
  (interactive)
  (run-command--log "Running selected command.")
  (if (use-region-p)
      (let ((command (buffer-substring-no-properties (region-beginning) (region-end))))
        (run-command--log "Selected command: %s" command)
        (run-command command))
    (run-command--log "No region selected.")))

;;; Unit tests for run-command functions using ERT

(ert-deftest run-command-test-buffer-policy-always ()
  "Test buffer policy 'always'."
  (let ((run-command-buffer-policy "always"))
    (should (equal run-command-buffer-policy "always"))))

(ert-deftest run-command-test-buffer-policy-never ()
  "Test buffer policy 'never'."
  (let ((run-command-buffer-policy "never"))
    (should (equal run-command-buffer-policy "never"))))

(ert-deftest run-command-test-buffer-policy-onSuccess ()
  "Test buffer policy 'onSuccess'."
  (let ((run-command-buffer-policy "onSuccess"))
    (should (equal run-command-buffer-policy "onSuccess"))))

(ert-deftest run-command-test-buffer-policy-onFailure ()
  "Test buffer policy 'onFailure'."
  (let ((run-command-buffer-policy "onFailure"))
    (should (equal run-command-buffer-policy "onFailure"))))

(ert-deftest run-command-test-exit-code-and-output ()
  "Test the exit code and output of specific commands."
  (let ((buffer-name "*run-command-output*")
        (run-command-buffer-policy "always"))
    ;; Success case: "echo test"
    (run-command "echo test")
    (with-current-buffer buffer-name
      (goto-char (point-max))
      (should (search-backward "test" nil t))
      (should (= (process-exit-status (get-buffer-process buffer-name)) 0)))
    ;; Failure case: "colmena fdh"
    (run-command "colmena fdh")
    (with-current-buffer buffer-name
      (goto-char (point-max))
      (should (search-backward "unrecognized subcommand" nil t))
      (should (/= (process-exit-status (get-buffer-process buffer-name)) 0)))))

(provide 'run-command)
;;; run-command.el ends here;;
;; echo test
;; colmena fh
;; (run-command-tests)
;; (ert t)
;;
;; (setq debug-on-error t)
;;
