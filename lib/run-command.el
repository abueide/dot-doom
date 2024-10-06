(defvar run-command-hide-default "onSuccess"
  "Default value for the hide argument in run-command.")

(defun run-selected-command (start end &optional hide)
  "Execute the selected region as a COMMAND asynchronously."
  (interactive "r")
  (let ((command (buffer-substring-no-properties start end)))
    (run-command command (or hide run-command-hide-default))))

(defun run-command (command &optional hide)
  "Execute COMMAND asynchronously."
  (interactive (list (read-string "Command: ")
                     (read-string "Hide output (always, never, onSuccess, onError) [onSuccess]: " nil nil run-command-hide-default)))
  (let* ((buffer-name "*command-output*")
         (buffer (get-buffer-create buffer-name)))
    (run-command-setup-buffer buffer)
    (let ((window (display-buffer-in-side-window buffer '((side . bottom)))))
      (run-command-setup-window window))
    (message "Starting process: %s" command)
    (run-command-start-process command buffer hide)))

(defun run-command-setup-buffer (buffer)
  "Prepare BUFFER by erasing contents and disabling read-only mode."
  (with-current-buffer buffer
    (read-only-mode -1)
    (erase-buffer)))

(defun run-command-setup-window (window)
  "Set up WINDOW by selecting it, entering normal state, and scrolling up."
  (select-window window)
  (when (fboundp 'evil-normal-state)
    (evil-normal-state))
  (goto-char (point-min)))

(defun run-command-start-process (command buffer hide)
  "Start COMMAND asynchronously and manage its output in BUFFER based on HIDE."
  (let ((process (start-process-shell-command "command-process" buffer command)))
    (set-process-sentinel process
                          (lexical-let ((hide (if (and hide (not (string= hide ""))) hide run-command-hide-default)))
                            (lambda (process event)
                              (message "Process event: %s" event)
                              (when (memq (process-status process) '(exit signal))
                                (run-command-handle-process-exit process hide)))))))

(defun run-command-handle-process-exit (process hide)
  "Handle PROCESS exit by managing the output buffer based on HIDE option."
  (let* ((exit-code (process-exit-status process))
         (buffer (process-buffer process)))
    (message "Process exited with code: %d" exit-code)
    (setq hide (downcase hide))
    (with-current-buffer buffer
      (ansi-color-apply-on-region (point-min) (point-max))
      (goto-char (point-min)))
    (pcase hide
      ((or "always" (and "onsuccess" (guard (zerop exit-code))))
       (run-command-cleanup buffer))
      ((or "never" (and "onerror" (guard (not (zerop exit-code)))))
       (run-command-handle-failure buffer))
      (_ (message "Invalid hide option: %s" hide)))))

(defun run-command-cleanup (buffer)
  "Clean up BUFFER and close its window."
  (when (get-buffer-window buffer)
    (delete-window (get-buffer-window buffer)))
  (kill-buffer buffer)
  (message "Process completed, buffer and window closed."))

(defun run-command-handle-failure (buffer)
  "Handle process failure by making BUFFER read-only."
  (with-current-buffer buffer
    (read-only-mode 1))
  (message "Process failed, buffer is read-only."))
