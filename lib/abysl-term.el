;;; abysl-term.el --- Terminal integration package -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides an interface for running terminal commands using either vterm or WezTerm.
;; It offers configuration options for terminal type, shell, and behavior for hiding terminal windows.
;;
;; Usage examples:
;; To use this package in your Emacs configuration, add the following to your config.el:
;;
;; (use-package! abysl-term
;;   :load-path "lib" ; Adjust the path as needed
;;   :custom
;;   (abysl-term-terminal 'vterm) ; 'vterm' or 'wezterm'
;;   (abysl-term-terminal-args '("start" "--always-new-process" "--")) ; Arguments for the terminal
;;   (abysl-term-shell 'bash) ; Shell to use: 'bash', 'zsh', 'fish', etc.
;;   (abysl-term-shell-args '("-l" "-i" "-c")) ; Shell arguments
;;   (abysl-term-hide 'onSuccess)) ; Hide terminal window: 'onSuccess', 'always', or 'never'

;; (abysl-term-run "colmena apply-local --sudo")
;; (abysl-term-run '("colmena" "apply-local" "--sudo"))
;; (abysl-term-run "colmena apply-local --sudo" 'wezterm '("start" "--") 'bash '("-c"))

;;; Code:

(defgroup abysl-term nil
  "Run terminal commands with vterm or WezTerm."
  :group 'tools)

(defcustom abysl-term-terminal "wezterm"
  "The terminal command to use, e.g., wezterm, vterm, etc."
  :type 'string
  :group 'abysl-term)

(defcustom abysl-term-terminal-args '("start" "--always-new-process" "--")
  "Arguments for the terminal."
  :type '(repeat string)
  :group 'abysl-term)

(defcustom abysl-term-shell "bash"
  "The shell to use, e.g., 'bash', 'zsh', 'fish'."
  :type 'string
  :group 'abysl-term)

(defcustom abysl-term-hide 'onSuccess
  "Hide terminal window: 'onSuccess', 'always', or 'never'."
  :type '(choice (const :tag "On Success" onSuccess)
          (const :tag "Always" always)
          (const :tag "Never" never))
  :group 'abysl-term)

(defcustom abysl-term-user-exit-hooks nil
  "A list of user-defined functions to run after the command completes.
Each function receives two arguments: exit code and command output."
  :type '(repeat function)
  :group 'abysl-term)

(defvar abysl-term--last-command nil
  "The last command run by `abysl-term-run'.")

;; Function for running a command
(defun abysl-term-run (command &optional terminal terminal-args shell current-exit-hook)
  "Run COMMAND in a terminal, with optional overrides for TERMINAL, TERMINAL-ARGS, and SHELL.
Optionally run a CURRENT-EXIT-HOOK for this specific command."
  (interactive)
  (setq abysl-term--last-command (list command terminal terminal-args shell current-exit-hook))
  (let* ((term (or terminal abysl-term-terminal))
         (term-args (or terminal-args abysl-term-terminal-args))
         (sh (or shell abysl-term-shell))
         ;; Flatten the full command into a single string
         (command-str (abysl-term--get-command-str command))
         ;; Generate temp script file, output file, and exit code file
         (temp-files (abysl-term--generate-command sh command-str))
         (script-file (nth 0 temp-files))
         (output-file (nth 1 temp-files))
         ;; Build the full command with wezterm, script file, and output handling
         (full-command
          (if (executable-find "script")
              (append (list term) term-args (list "script" output-file "-c" script-file))
            ;; When the condition is false (using `tee`)
            (append (list term) term-args (list sh "-c" (format "stdbuf -oL -eL %s 2>&1 | tee %s" script-file output-file)))
            ))
         ;; Merge user-defined exit hooks with the optional current exit hook
         (exit-hooks (abysl-term--merge-exit-hooks abysl-term-user-exit-hooks current-exit-hook)))
    ;; Run the terminal process
    (abysl-term--run-terminal full-command temp-files exit-hooks)))
;; Run the currently selected text as a command
(defun abysl-term-run-selected ()
  "Run the currently selected text as a command."
  (interactive)
  (if (use-region-p)
      (let ((command (buffer-substring-no-properties (region-beginning) (region-end))))
        (abysl-term-run command))
    (message "No region selected")))

;; Run the previous command
(defun abysl-term-run-previous ()
  "Run the previous command with the same arguments."
  (interactive)
  (if abysl-term--last-command
      (progn
        (apply 'abysl-term-run abysl-term--last-command))
    (message "No previous command to run")))

;; Internal helpers

(defun abysl-term--run-terminal (full-command tmp-files exit-hooks)
  "Run FULL-COMMAND in the terminal, handling process lifecycle using TMP-FILES.
Run EXIT-HOOKS after process completion."
  ;; Print tmp-files for debugging
  ;; Create the process using `make-process`
  (make-process
   :name "*abysl-term-output*"
   :buffer "*abysl-term-output*"
   :command full-command
   :sentinel (lambda (proc event) (abysl-term--process-sentinel proc event tmp-files exit-hooks))
   )
  )

(defun abysl-term--process-sentinel (proc event tmp-files exit-hooks)
  "Sentinel function to handle process PROC events.
Runs EXIT-HOOKS after process completion and processes output and exit code from TMP-FILES."
  ;; Proceed when the process finishes
  (when (string= event "finished\n")
    (let ((sentinel-exit-code (process-exit-status proc)))  ;; Capture sentinel exit code
      (abysl-term--default-exit-hook tmp-files)
      (if (not (eq sentinel-exit-code 0))
          (error "[abysl-term--process-sentinel] Sentinel exit code: %d" sentinel-exit-code)))))

(defun abysl-term--default-exit-hook (tmp-files)
  "Default exit hook to process the TMP-FILES and call user-defined exit hooks."
  (let* ((output-file (nth 1 tmp-files))
         (exit-code-file (nth 2 tmp-files))
         (exit-codes (mapcar #'string-to-number
                             (split-string (with-temp-buffer
                                             (insert-file-contents exit-code-file)
                                             (buffer-string)))))
         (output (with-temp-buffer
                   (insert-file-contents output-file)
                   (buffer-string))))
    ;; Clean up all temp files by iterating through the list
    ;; (dolist (file tmp-files)
    ;;   (when (file-exists-p file)
    ;;     (delete-file file)
    ;;     ))
    ;; Handle output based on exit-codes
    (abysl-term--handle-output exit-codes output)
    ;; Call user-defined hooks
    (dolist (hook abysl-term-user-exit-hooks)
      (funcall hook exit-codes output))))


(defun abysl-term--show-buffer (exit-codes output)
  "Create and display the buffer with EXIT-CODES and OUTPUT, stripping carriage returns."
  (let ((buffer (get-buffer-create "*abysl-term-output*"))
        (cleaned-output (abysl-term--strip-carriage-returns output)))
    (with-current-buffer buffer
      (read-only-mode -1)  ;; Disable read-only before inserting content
      (erase-buffer)
      (insert (propertize (format "Exit Codes: %s\nOutput:\n"
                                  (mapconcat #'number-to-string exit-codes " "))
                          'face 'font-lock-comment-face))
      (insert cleaned-output)
      (ansi-color-apply-on-region (point-min) (point-max))
      (read-only-mode 1))  ;; Enable read-only after inserting content
    ;; Open the buffer in a bottom window
    (pop-to-buffer buffer '((display-buffer-at-bottom)))))

(defun abysl-term--handle-output (exit-codes output)
  "Handle displaying OUTPUT based on EXIT-CODES and the `abysl-term-hide` setting."
  (cond
   ;; If the setting is 'never', do nothing
   ((eq abysl-term-hide 'never)
    (message "[abysl-term--handle-hide] Output hidden due to 'never' setting"))

   ;; If the setting is 'always', show the buffer
   ((eq abysl-term-hide 'always)
    (abysl-term--show-buffer exit-codes output))

   ;; If the setting is 'onSuccess' and the command failed, show the buffer
   ((and (eq abysl-term-hide 'onSuccess)
         (cl-some (lambda (code) (/= code 0)) exit-codes))
    (abysl-term--show-buffer exit-codes output))))

(defun abysl-term--merge-exit-hooks (user-hooks current-exit-hook)
  "Merge USER-HOOKS with the CURRENT-EXIT-HOOK.
If both are nil, return an empty list to ensure safe iteration."
  (let ((hooks (append (or user-hooks '()) (when current-exit-hook (list current-exit-hook)))))
    hooks))

(defun abysl-term--generate-command (shell command-str)
  "Generate temporary files for the command, output, and exit code, and write the command to a temporary script file."
  (let* ((full-shell (abysl-term--find-shell-path shell))  ;; Get full shell path
         (script-file (make-temp-file "command-"))
         (output-file (make-temp-file "command-output-"))
         (exit-code-file (make-temp-file "command-exit-code-")))
    ;; Write the shell command to the script file
    (with-temp-file script-file
      ;; Add the shebang with the full shell path
      (insert (format "#!%s\n" full-shell))
      ;; Add shell arguments and the command
      (insert (format "%s\n" command-str))  ;; User command
      ;; Add exit code capturing logic
      (insert (if (string-match-p "fish" full-shell)
                  (format "echo $pipestatus > %s\n" exit-code-file)
                (format "echo ${PIPESTATUS[*]} > %s\n" exit-code-file))))
    ;; Make the script file executable
    (set-file-modes script-file #o755)
    ;; Return the list of the script file, output file, and exit code file
    (list script-file output-file exit-code-file)))

(defun abysl-term--find-shell-path (shell)
  "Find the full path of the SHELL if it's just a shell name, otherwise return the shell."
  (if (file-name-absolute-p shell)
      shell  ;; If shell is already an absolute path, return it
    (or (executable-find shell)  ;; Look up shell in PATH
        (error "Shell '%s' not found in PATH" shell))))  ;; Error if not found

(defun abysl-term--get-command-str (command)
  "Convert COMMAND to a string if it is a list."
  (if (listp command)
      (mapconcat 'identity command " ")
    command))

(defun abysl-term--message-tmp-files (tmp-files prefix)
  "Print the paths and contents of TMP-FILES for debugging purposes with a PREFIX label.
Warns if any file doesn't exist."
  (dolist (file tmp-files)
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (message "[%s] File: %s\nContents:\n%s" prefix file (buffer-string)))
      (message "[%s] Warning: %s doesn't exist" prefix file))))

(defun abysl-term--strip-carriage-returns (string)
  "Remove carriage returns (^M) from STRING."
  (replace-regexp-in-string "\r" "" string))

(provide 'abysl-term)
;;; abysl-term.el ends here
