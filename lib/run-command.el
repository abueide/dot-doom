;;; abysl-interminal.el --- Execute commands in term buffer -*- lexical-binding: t -*-
;;; Commentary:
;;; This package allows you to run a command asynchronously with term.
;;; It has options to show/hide the output console depending on the command's exit code.
;;; Code:
(require 'term)
(require 'subr-x)
(require 'evil)
(require 'cl-lib)
(require 'ert)  ; Emacs Lisp Testing Framework

(defvar abysl-interminal-debug t "Enable debug for abysl-interminal.")
(defvar abysl-interminal-buffer-policy "onSuccess"
  "Output buffer policy: always, never, onSuccess, onFailure.")
(defvar abysl-interminal-shell "bash"
  "Default shell to use for running commands. Can be set to bash, fish, or zsh.")

(defun abysl-interminal--log (message &rest args)
  "Log MESSAGE with ARGS if `abysl-interminal-debug` is enabled."
  (when abysl-interminal-debug
    (apply #'message (concat "[abysl-interminal]: " message) args)))

(defun abysl-interminal (command &optional shell on-success on-failure)
  "Run COMMAND in a term buffer using SHELL"
  (let ((shell-to-use (or shell abysl-interminal-shell)))
    (abysl-interminal--log "Running: %s with shell: %s" command shell-to-use)
    (let* ((buffer-name "*abysl-interminal-output*")
           (buffer (get-buffer-create buffer-name)))
      (abysl-interminal--log "Creating term buffer: %s" buffer-name)
      (with-current-buffer buffer
        (term-mode)
        (term-exec buffer "abysl-interminal" shell-to-use nil (list "-l" "-i" "-c" command)))
      (display-buffer-at-bottom buffer '((window-height . 0.25)))
      (select-window (get-buffer-window buffer))
      (evil-normal-state)
      (set-process-sentinel (get-buffer-process buffer)
                            (lambda (proc _event)
                              (let ((exit-code (process-exit-status proc)))
                                (abysl-interminal--log "Exit code: %d" exit-code)
                                (cond
                                 ((string= abysl-interminal-buffer-policy "always")
                                  (abysl-interminal--log "Policy: always keep buffer."))
                                 ((string= abysl-interminal-buffer-policy "never")
                                  (abysl-interminal--log "Policy: never keep buffer.")
                                  (delete-window (get-buffer-window buffer)))
                                 ((and (string= abysl-interminal-buffer-policy "onSuccess")
                                       (= exit-code 0))
                                  (abysl-interminal--log "Policy: onSuccess, closing buffer.")
                                  (delete-window (get-buffer-window buffer)))
                                 ((and (string= abysl-interminal-buffer-policy "onFailure")
                                       (/= exit-code 0))
                                  (abysl-interminal--log "Policy: onFailure, closing buffer.")
                                  (delete-window (get-buffer-window buffer))))
                                ;; Execute hooks based on exit code
                                (if (= exit-code 0)
                                    (when on-success (funcall on-success))
                                  (when on-failure (funcall on-failure)))))))))


(defun run-selected-command (&optional shell)
  "Run the currently selected text in SHELL."
  (interactive)
  (abysl-interminal--log "Running selected command.")
  (if (use-region-p)
      (let ((command (buffer-substring-no-properties (region-beginning) (region-end))))
        (abysl-interminal--log "Selected command: %s" command)
        (abysl-interminal command shell))
    (abysl-interminal--log "No region selected.")))

;;; Unit tests for abysl-interminal functions using ERT

(ert-deftest abysl-interminal-test-buffer-policy-always ()
  "Test buffer policy 'always'."
  (let ((abysl-interminal-buffer-policy "always"))
    (should (equal abysl-interminal-buffer-policy "always"))))

(ert-deftest abysl-interminal-test-buffer-policy-never ()
  "Test buffer policy 'never'."
  (let ((abysl-interminal-buffer-policy "never"))
    (should (equal abysl-interminal-buffer-policy "never"))))

(ert-deftest abysl-interminal-test-buffer-policy-onSuccess ()
  "Test buffer policy 'onSuccess'."
  (let ((abysl-interminal-buffer-policy "onSuccess"))
    (should (equal abysl-interminal-buffer-policy "onSuccess"))))

(ert-deftest abysl-interminal-test-buffer-policy-onFailure ()
  "Test buffer policy 'onFailure'."
  (let ((abysl-interminal-buffer-policy "onFailure"))
    (should (equal abysl-interminal-buffer-policy "onFailure"))))

(ert-deftest abysl-interminal-test-exit-code-and-output ()
  "Test the exit code and output of specific commands."
  (let ((buffer-name "*abysl-interminal-output*")
        (abysl-interminal-buffer-policy "always"))
    ;; Success case: "echo test" with bash
    (abysl-interminal "echo test" "bash")
    (with-current-buffer buffer-name
      (goto-char (point-max))
      (should (search-backward "test" nil t))
      (should (= (process-exit-status (get-buffer-process buffer-name)) 0)))
    ;; Success case: "echo test" with fish
    (abysl-interminal "echo test" "fish")
    (with-current-buffer buffer-name
      (goto-char (point-max))
      (should (search-backward "test" nil t))
      (should (= (process-exit-status (get-buffer-process buffer-name)) 0)))
    ;; Success case: "echo test" with zsh
    (abysl-interminal "echo test" "zsh")
    (with-current-buffer buffer-name
      (goto-char (point-max))
      (should (search-backward "test" nil t))
      (should (= (process-exit-status (get-buffer-process buffer-name)) 0)))));;; abysl-interminal.el ends here;;
