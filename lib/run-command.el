(defun run-external-command (command)
  "Run COMMAND using WezTerm with bash -l -i -c, pipe output to a temporary file, and print a message when the process finishes."
  (interactive "sCommand to run: ")
  (let ((output-file (make-temp-file "wezterm-output-"))
        (exit-code-file (make-temp-file "wezterm-exit-code-")))
    (message "Temporary output file created at: %s" output-file)
    (message "Temporary exit code file created at: %s" exit-code-file)
    (make-process
     :name "wezterm-process"
     :command `("wezterm" "start" "bash" "-l" "-i" "-c" ,(format "%s > %s 2>&1; echo $? > %s" command output-file exit-code-file))
     :sentinel (run-external-command--create-sentinel output-file exit-code-file))))

(defun run-external-command--create-sentinel (output-file exit-code-file)
  "Create a sentinel function to handle the process events and use OUTPUT-FILE and EXIT-CODE-FILE."
  (lambda (process event)
    (when (string= event "finished\n")
      (message "Command finished. Output written to: %s" output-file)
      (message "Exit code written to: %s" exit-code-file))))


;; echo test
;; colmena fdh
