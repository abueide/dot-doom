;;; run-command.el --- Execute commands in term buffer -*- lexical-binding: t -*-
(require 'term)
(require 'subr-x)
(require 'evil)
(require 'cl-lib)
(require 'ert)  ; Emacs Lisp Testing Framework

(defvar run-command-debug t "Enable debug statements for run-command functions.")

(defun run-command--log (message &rest args)
  "Log MESSAGE with ARGS if `run-command-debug` is enabled."
  (when run-command-debug
    (apply #'message (concat "[run-command]: " message) args)))

(defun run-command (command)
  "Run COMMAND in a term buffer at the bottom of the screen.

COMMAND is executed using `bash -l` in interactive mode.
If the command succeeds, the term window is closed, otherwise it stays open."
  (run-command--log "Starting run-command with command: %s" command)
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
                            (let ((exit-code (process-exit-status proc))
                                  (output (with-current-buffer (process-buffer proc)
                                            (string-trim (buffer-string)))))
                              (run-command--log "Process finished with exit code: %d" exit-code)
                              (if (= exit-code 0)
                                  (progn
                                    (run-command--log "Command succeeded, closing window.")
                                    (delete-window (get-buffer-window buffer)))
                                (run-command--log "Command failed, keeping window open."))
                              (message "Command output: %s\nExit code: %d" output exit-code))))))

(defun run-selected-command ()
  "Run the currently selected text as a command using `run-command`."
  (interactive)
  (run-command--log "Starting run-selected-command.")
  (if (use-region-p)
      (let ((command (buffer-substring-no-properties (region-beginning) (region-end))))
        (run-command--log "Selected command: %s" command)
        (run-command command))
    (run-command--log "No region selected.")))

;;; Unit tests for run-command functions using ERT

(defun run-command-test--setup ()
  "Setup function for run-command unit tests."
  (setq run-command-debug nil))

(defun run-command-test--teardown ()
  "Teardown function for run-command unit tests."
  (setq run-command-debug t))

(defun run-command-test--execute-and-check (command expected-output expected-exit-code &optional input)
  "Execute COMMAND and verify the output and exit code.

COMMAND is executed in a term buffer, and the output and exit code
are compared against EXPECTED-OUTPUT and EXPECTED-EXIT-CODE. If INPUT is
provided, it will be sent to the process during execution."
  (let* ((buffer-name "*run-command-test*")
         (buffer (get-buffer-create buffer-name))
         (finished nil)
         (output nil)
         (exit-code nil))
    (with-current-buffer buffer
      (term-mode)
      (term-exec buffer "run-command-test" "bash" nil (list "-l" "-i" "-c" command)))
    (display-buffer-at-bottom buffer '((window-height . 0.25)))
    (select-window (get-buffer-window buffer))
    (when input
      (run-command--log "Sending input: %s" input)
      (term-send-raw-string (concat input "\n")))
    (set-process-sentinel (get-buffer-process buffer)
                          (lambda (proc _event)
                            (setq exit-code (process-exit-status proc))
                            (setq output (with-current-buffer (process-buffer proc)
                                           (string-trim (buffer-string))))
                            (setq finished t)))
    (let ((timeout 10) (elapsed 0))
      (while (and (not finished) (< elapsed timeout))
        (sit-for 0.1)
        (setq elapsed (+ elapsed 0.1)))
      (unless finished
        (run-command--log "Timeout reached while waiting for command to complete.")
        (setq finished t)))
    (ert-info ((format "Testing command: %s" command))
      (should (string-match-p expected-output output))  ; Check if output contains the expected substring
      (should (= exit-code expected-exit-code)))))  ; Check if exit code matches expected value

(ert-deftest run-command-test-echo ()
  "Test that `echo test` returns `test` and exit code 0."
  (run-command-test--execute-and-check "echo test" "test" 0))

(ert-deftest run-command-test-invalid-command ()
  "Test that `colmena fdh` returns a non-zero exit code."
  (run-command-test--execute-and-check "colmena fdh" "unrecognized subcommand" 2))

(ert-deftest run-command-test-user-input ()
  "Test a command that requires user input.

The command reads input and echoes it. The test ensures the echoed
output matches the input provided."
  (run-command-test--execute-and-check "read -p 'Enter something: ' input; echo $input" "test" 0 "test"))

(defun run-command-tests ()
  "Run all unit tests for run-command functions."
  (interactive)
  (run-command-test--setup)
  (unwind-protect
      (progn
        (ert-run-tests-interactively t))
    (run-command-test--teardown)))

(provide 'run-command)

;;; run-command.el ends here;; echo test
;; colmena fh
;; (run-command-tests)
;; (ert t)
;;
;; (setq debug-on-error t)
;;
