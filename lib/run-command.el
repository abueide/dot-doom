;; -*- lexical-binding: t; -*-
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
  (message (format "received output-file %s" output-file))
  (message (format "received exit-code-file %s" exit-code-file))
  (lambda (process event)
    (when (string= event "finished\n")
      (let ((exit-code (with-temp-buffer
                         (insert-file-contents exit-code-file)
                         (string-to-number (buffer-string))))
            (command-output (with-temp-buffer
                              (insert-file-contents output-file)
                              (buffer-string))))
        (message "%s" exit-code)
        (message "%s" command-output)
        (delete-file output-file)
        (delete-file exit-code-file)
        (run-external-command--exit-hook exit-code command-output)
        )
      )))

(defun run-external-command--exit-hook (exit-code command-output)
  "Default exit hook that handles non-zero EXIT-CODE by displaying COMMAND-OUTPUT in a buffer."
  (if (/= exit-code 0)
      (with-current-buffer (get-buffer-create "*Command Output*")
        (erase-buffer)
        (insert (format "Command exited with code %d\n\n" exit-code))
        (insert command-output)
        (display-buffer (current-buffer)))
    (message "Command completed successfully.")))

(defun run-selected-command ()
  "Run the currently selected text as a command using `run-external-command'."
  (interactive)
  (if (use-region-p)
      (let ((command (buffer-substring-no-properties (region-beginning) (region-end))))
        (run-external-command command))
    (message "No region selected.")))



;; echo test
;; colmena fdh
