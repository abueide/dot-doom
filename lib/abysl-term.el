;;; abysl-term.el --- Terminal integration package -*- lexical-binding: t; -*-

;; Commentary:
;; This package provides an interface for running terminal commands using either vterm or WezTerm.
;; It offers configuration options for terminal type, shell, and behavior for hiding terminal windows.

;; Code:
(defgroup abysl-term nil
  "Run terminal commands with vterm or WezTerm."
  :group 'tools)

(defcustom abysl-term-terminal 'vterm
  "The terminal to use: 'vterm' or 'wezterm'."
  :type '(choice (const :tag "vterm" vterm)
          (const :tag "WezTerm" wezterm))
  :group 'abysl-term)

(defcustom abysl-term-terminal-args '("start" "--")
  "Arguments for the terminal."
  :type '(repeat string)
  :group 'abysl-term)

(defcustom abysl-term-shell 'bash
  "The shell to use: 'bash', 'zsh', 'fish', etc."
  :type '(choice (const :tag "bash" bash)
          (const :tag "zsh" zsh)
          (const :tag "fish" fish))
  :group 'abysl-term)

(defcustom abysl-term-shell-args '("-l" "-i" "-c")
  "Arguments for the shell."
  :type '(repeat string)
  :group 'abysl-term)

(defcustom abysl-term-hide 'onSuccess
  "Hide terminal window: 'onSuccess', 'always', or 'never'."
  :type '(choice (const :tag "On Success" onSuccess)
          (const :tag "Always" always)
          (const :tag "Never" never))
  :group 'abysl-term)

;; Function for running a command
(defun abysl-term-run (command &optional terminal terminal-args shell shell-args)
  "Run COMMAND in a terminal, with optional overrides for TERMINAL, TERMINAL-ARGS, SHELL, and SHELL-ARGS."
  (let ((term (or terminal abysl-term-terminal))
        (term-args (or terminal-args abysl-term-terminal-args))
        (sh (or shell abysl-term-shell))
        (sh-args (or shell-args abysl-term-shell-args)))
    ;; Implementation logic goes here
    (make-process
     :name "wezterm-process"
     :command `("wezterm" "start" "--" ,shell-to-use "-l" "-i" "-c" ,formatted-command)
                                        ;     :command `("wezterm" "start" "--" ,shell-to-use "-l" "-i" "-c"
                                        ;                ,(run-command--format-command shell-to-use command exit-code-file output-file))
     :sentinel (run-command--create-sentinel output-file exit-code-file))
    ))

;; Function for running a command as a list of arguments
(defun abysl-term-run-args (args &optional terminal terminal-args shell shell-args)
  "Run a command as a list of ARGS, with optional overrides for TERMINAL, TERMINAL-ARGS, SHELL, and SHELL-ARGS."
  (apply 'abysl-term-run (mapconcat 'identity args " ") terminal terminal-args shell shell-args))

;; Run the currently selected text as a command
(defun abysl-term-run-selected ()
  "Run the currently selected text as a command."
  (interactive)
  (if (use-region-p)
      (let ((command (buffer-substring-no-properties (region-beginning) (region-end))))
        (abysl-term-run command))
    (message "No region selected")))

(provide 'abysl-term)
;;; abysl-term.el ends here

;; Commented-out examples of usage
;; (use-package! abysl-term
;;   :custom
;;   (abysl-term-terminal 'vterm) ; 'vterm' or 'wezterm'
;;   (abysl-term-terminal-args '("start" "--always-new-process" "--")) ; Arguments for the terminal
;;   (abysl-term-shell 'bash) ; Shell to use: 'bash', 'zsh', 'fish', etc.
;;   (abysl-term-shell-args '("-l" "-i" "-c")) ; Shell arguments
;;   (abysl-term-hide 'onSuccess)) ; Hide terminal window: 'onSuccess', 'always', or 'never'

;; (abysl-term-run "colmena apply-local --sudo")
;; (abysl-term-run '("colmena" "apply-local" "--sudo"))
;; (abysl-term-run "colmena apply-local --sudo" 'wezterm '("start" "--") 'bash '("-c"));;; abysl-term.el --- Terminal integration package -*- lexical-binding: t; -*-

(defun run-command (command &optional terminal shell-to-use)
  "Run COMMAND using WezTerm with bash -l -i -c, pipe output to a temporary file, and print a message when the process finishes. Color codes are preserved."
  (interactive "sCommand to run: ")
  (let* ((output-file (make-temp-file "wezterm-output-"))
         (exit-code-file (make-temp-file "wezterm-exit-code-"))
         (shell-to-use (or shell-to-use "bash"))
         (terminal (or terminal "wezterm"))
         (formatted-command (format "'echo $BASH_VERSION > %s'" output-file))
         )
    (message "Temporary output file created at: %s" output-file)
    (message "Temporary exit code file created at: %s" exit-code-file)
    (message formatted-command)
    ))

(defun run-command--format-command (terminal shell command)
  (cond
   ((string-match "wezterm" terminal)
    (format "%s start %s" terminal shell)
    )
   ;; For Fish shell
   ((string-match "fish" shell-to-use)
    (format "script -q -c '%s; set exit_code $status; echo $exit_code > %s' /dev/null | tee %s"
            command exit-code-file output-file))
   ;; For Bash or similar shells
   ((string-match "bash\\|sh" shell-to-use)
    (format "script -q -c '%s; exit_code=$?; echo $exit_code > %s' /dev/null | tee %s"
            command exit-code-file output-file))
   ;; Fallback if an unsupported shell is detected
   (t
    (error "Unsupported shell type: %s" shell-to-use))))

(defun run-command--create-sentinel (output-file exit-code-file)
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
                                        ;(delete-file output-file)
                                        ;(delete-file exit-code-file)
        (run-command--exit-hook exit-code command-output)
        )
      )))


(defun run-command--remove-carriage-returns (text)
  "Remove carriage return characters (^M) from TEXT."
  (replace-regexp-in-string "\r" "" text))

(defun run-command--create-output-buffer (buffer-name)
  "Create a buffer that acts like a temporary minibuffer but displays colored output."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (local-set-key (kbd "q") #'bury-buffer)
      (local-set-key (kbd "C-g") #'bury-buffer)
      ;; Track cursor movement and close buffer if cursor moves
      (let ((initial-point (point)))
        (add-hook 'post-command-hook
                  (lambda ()
                    (when (and (get-buffer-window buffer)
                               (/= (point) initial-point))
                      (bury-buffer buffer)))
                  nil t)))
    buffer))

(defun run-command--exit-hook (exit-code command-output)
  "Default exit hook that handles non-zero EXIT-CODE by displaying COMMAND-OUTPUT in a temporary buffer with color."
  (let* ((clean-output (run-command--remove-carriage-returns command-output))
         (buffer-name "*Command Output*")
         (buffer (run-command--create-output-buffer buffer-name)))
    (if (/= exit-code 0)
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert (format "Command exited with code %d:\n\n%s" exit-code clean-output))
            ;; Apply ANSI color processing after inserting the text
            (ansi-color-apply-on-region (point-min) (point-max))
            ;; Pop the buffer to display the output
            (pop-to-buffer buffer)))
      (message "Command completed successfully."))))


(defun run-selected-command ()
  "Run the currently selected text as a command using `run-command'."
  (interactive)
  (if (use-region-p)
      (let ((command (buffer-substring-no-properties (region-beginning) (region-end))))
        (run-command command))
    (message "No region selected.")))


;; echo test
;; colmena fdh
;; colmena apply-local --sudo
