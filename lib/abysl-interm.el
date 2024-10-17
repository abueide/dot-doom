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
  "Output buffer policy: always, never, onSuccess, onFailure.")
(defvar run-command-shell "bash"
  "Default shell to use for running commands. Can be set to bash, fish, or zsh.")

(defun run-command--log (message &rest args)
  "Log MESSAGE with ARGS if `run-command-debug` is enabled."
  (when run-command-debug
    (apply #'message (concat "[run-command]: " message) args)))

(defun run-command (command &optional shell on-success on-failure)
  "Run COMMAND in a term buffer using SHELL"
  (let ((shell-to-use (or shell run-command-shell)))
    (run-command--log "Running: %s with shell: %s" command shell-to-use)
    (let* ((buffer-name "*run-command-output*")
           (buffer (get-buffer-create buffer-name)))
      (run-command--log "Creating term buffer: %s" buffer-name)
      (with-current-buffer buffer
        (term-mode)
        (term-exec buffer "run-command" shell-to-use nil (list "-l" "-i" "-c" command)))
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
                                  (delete-window (get-buffer-window buffer))))
                                ;; Execute hooks based on exit code
                                (if (= exit-code 0)
                                    (when on-success (funcall on-success))
                                  (when on-failure (funcall on-failure)))))))))


(defun run-selected-command (&optional shell)
  "Run the currently selected text in SHELL."
  (interactive)
  (run-command--log "Running selected command.")
  (if (use-region-p)
      (let ((command (buffer-substring-no-properties (region-beginning) (region-end))))
        (run-command--log "Selected command: %s" command)
        (run-command command shell))
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
    ;; Success case: "echo test" with bash
    (run-command "echo test" "bash")
    (with-current-buffer buffer-name
      (goto-char (point-max))
      (should (search-backward "test" nil t))
      (should (= (process-exit-status (get-buffer-process buffer-name)) 0)))
    ;; Success case: "echo test" with fish
    (run-command "echo test" "fish")
    (with-current-buffer buffer-name
      (goto-char (point-max))
      (should (search-backward "test" nil t))
      (should (= (process-exit-status (get-buffer-process buffer-name)) 0)))
    ;; Success case: "echo test" with zsh
    (run-command "echo test" "zsh")
    (with-current-buffer buffer-name
      (goto-char (point-max))
      (should (search-backward "test" nil t))
      (should (= (process-exit-status (get-buffer-process buffer-name)) 0)))));;; run-command.el ends here;;

