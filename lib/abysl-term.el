;;; abysl-term.el --- Terminal integration package -*- lexical-binding: t; -*-

;;; Commentary:
;;(use-package! abysl-term
;; :load-path "lib"
;; :custom
;; ;; Set default terminal
;; (abysl-term-terminal "wezterm")
;; ;; Set default terminal args
;; (abysl-term-terminal-args (list "--always-new-process" "--workspace" "AbyslWorkspace"))
;; ;; Set default shell
;; (abysl-term-shell "fish")
;; ;; When to hide the buffer that opens containing the command's output after it completes
;; (abysl-term-hide 'onSuccess)
;; ;; Extend the `abysl-term-args-alist` to add support for arbitrary terminal emulators.
;; ;; Please consider making a PR after testing :)
;; (abysl-term-args-alist
;;  (append abysl-term-args-alist
;;          ;; Flag to set the working directory.
;;          ;; By default uses projectile-project-root
;;          ;; or if unavailable current buffer's directory
;;          ;; or if unavailable user's home directory
;;          ;; The terminal name tag must match the terminal's binary name in the PATH
;;          '((wezterm . ((cd . "--work-dir")
;;                        ;; Subcommand to launch an executable within the terminal (if any) Starting command (if any)
;;                        (start . "start")
;;                        ;; Use '--' separator to separate terminal args from program args.
;;                        (use-argument-separator . t))))))
;; :config
;; (map! :leader
;;       :prefix ("r" . "run")
;;       :desc "Run selected text"
;;       "c" #'abysl-term-run-selected)
;; (map! :leader
;;       :prefix ("r" . "run")
;;       :desc "Run previous command"
;;       "p" #'abysl-term-run-previous)
;; ;; Opens your default shell in your default terminal
;; (map! :leader
;;       :prefix ("o" . "open")
;;       :desc "[o]pen abysl-term [s]hell"
;;       "s" #'abysl-term-open-shell)
;; (map! :leader
;;       :desc "Sync Doom and Restart"
;;       :nv
;;       "q t" (lambda ()
;;               (interactive)
;;               ;; Example of adding a custom exit hook to an invocation of abysl-term-run
;;               ;; exit-codes contains an exit code for each command in your pipe
;;               ;; Pretend there is a command called simulate-error which returns an argument as an error code
;;               ;; simulate-error 0 | simulate-error 44 | simulate-error 25
;;               ;; would make exit codes = (list 0 44 25) when the command completes
;;               ;; cl-every ensures we only restart if there are no errors so we can see them if they happen
;;               (abysl-term-run "doom sync"
;;                               (lambda (exit-codes output)
;;                                 (message "exit hook called")
;;                                 (if (cl-every (lambda (x) (eq x 0)) exit-codes)
;;                                     (doom/restart-and-restore))))))
;; (map! :leader
;;       :mode nix-mode
;;       :prefix ("c" . "colmena")
;;       :desc "Colmena apply-local --sudo" "l"
;;       (lambda ()
;;         (interactive)
;;         (abysl-term-run "colmena apply-local --sudo"))

;;       :desc "Colmena apply with tags" "a"
;;       (lambda ()
;;         (interactive)
;;         (let* ((tags (read-string "Enter tags (space or comma separated): "))
;;                (formatted-tags (mapconcat 'identity (split-string tags "[ ,]+" t) ",")))
;;           (abysl-term-run (format "colmena apply --on %s" formatted-tags)))))
;; )
;;;
;;; Code:

(defgroup abysl-term nil
  "The Emacs interface for interacting with external terminals."
  :group 'tools)

(defcustom abysl-term-terminal "wezterm"
  "The terminal command to use, e.g., wezterm, vterm, etc."
  :type 'string
  :group 'abysl-term)

(defcustom abysl-term-terminal-args '("--always-new-process")
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

(defcustom abysl-term-args-alist
  '((wezterm . ((cd . "--cwd")
                (start . "start")
                (use-argument-separator . t)))
    (alacritty . ((cd . "--working-directory")
                  (use-argument-separator . nil)))
    (git-bash . ((cd . "--cd")
                 (use-argument-separator . nil)))
    (mintty . ((cd . "--cwd")
               (start . "start --")
               (use-argument-separator . nil)))
    (kitty . ((cd . "--directory")
              (use-argument-separator . nil)))
    (zellij . ((cd . "cd")
               (use-argument-separator . nil))))
  "User-configurable alist mapping terminals to their argument flags for different actions like 'cd and 'start."
  :type '(alist :key-type (symbol :tag "Terminal name")
          :value-type (alist :key-type (symbol :tag "Action")
                             :value-type (sexp :tag "Argument flag")))
  :group 'abysl-term)


(defcustom abysl-term-user-exit-hooks nil
  "A list of user-defined functions to run after the command completes.
Each function receives two arguments: exit code and command output."
  :type '(repeat function)
  :group 'abysl-term)

(defvar abysl-term--last-command nil
  "The last command run by `abysl-term-run'.")

;; Function for running a command
(defun abysl-term-open-shell (&optional terminal terminal-args shell)
  "Opens a new SHELL using TERMINAL with TERMINAL-ARGS."
  (interactive)
  (let*(
        (term (or terminal abysl-term-terminal))
        (term-args (or terminal-args abysl-term-terminal-args))
        (sh (or shell abysl-term-shell))
        (command (abysl-term--format-command term term-args sh (list "-li"))
                 ))
    (make-process
     :name "*abysl-term-shell*"
     :buffer "*abysl-term-shell*"
     :command command)
    )
  )

(defun abysl-term-run (command &optional current-exit-hook terminal terminal-args shell)
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
              (abysl-term--format-command term term-args "script" (list "--flush" output-file "-c" script-file))
            ;; When the condition is false (using `tee`)
            (abysl-term--format-command
             term term-args sh (list "-lc")
             (format
              "stdbuf -oL -eL %s 2>&1 | tee %s"
              (abysl-term--convert-to-unix-path script-file)
              (abysl-term--convert-to-unix-path output-file)
              ))
            ))
         ;; Merge user-defined exit hooks with the optional current exit hook
         (exit-hooks (abysl-term--merge-exit-hooks abysl-term-user-exit-hooks current-exit-hook)))
    ;; Run the terminal process
    (abysl-term--run-terminal full-command temp-files exit-hooks)))
;; Run the currently selected text as a command
;; colmena fdh
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
(defun abysl-term--get-terminal-flags (term action)
  "Retrieve terminal-specific argument flags for TERM and ACTION from `abysl-term-args-alist`. Always returns a string or nil."
  (let ((term-entry (assoc (intern term) abysl-term-args-alist)))
    (if term-entry
        (let ((flag (cdr (assoc action (cdr term-entry)))))
          (if (stringp flag)
              flag
            nil))  ;; Ensure only a string or nil is returned
      (error "Unsupported terminal: %s. Please add it to `abysl-term-args-alist`.\nExample: (add-to-list 'abysl-term-args-alist '(%s . ((cd . \"--your-cd-flag\") (start . \"--your-start-flag\"))))" term term))))

(defun abysl-term--use-argument-separator (term)
  "Return a list with '--' if TERM needs a separator, or an empty list if not."
  (let ((term-entry (assoc (intern term) abysl-term-args-alist)))
    (if term-entry
        (let ((separator (cdr (assoc 'use-argument-separator (cdr term-entry)))))
          (if separator
              '("--")
            '()))  ;; Return an empty list if no separator is needed
      '())))


(defun abysl-term--format-command (term &optional term-args shell shell-args command)
  "Prepare the command and arguments for `make-process` based on TERM, TERM-ARGS, SHELL, SHELL-ARGS, and COMMAND.
If both SHELL and COMMAND are nil, an error is thrown."
  ;; Error if both shell and command are nil
  (when (and (null shell) (null command))
    (error "Both shell and command cannot be nil"))

  (let* ((dir (abysl-term--get-project-root-or-default))
         (cd-arg (abysl-term--get-terminal-flags term 'cd))
         (start-arg (abysl-term--get-terminal-flags term 'start))
         (separator-arg (abysl-term--use-argument-separator term)))

    ;; Construct the full command
    (let ((formatted-command
           (append (list term)
                   (when start-arg (list start-arg))  ;; Add start subcommand if required
                   (when term-args term-args)  ;; Terminal arguments before the '--'
                   (when cd-arg (list cd-arg dir))  ;; Add the directory argument if possible
                   separator-arg  ;; Add '--' separator if required
                   (when shell (list shell))  ;; Add shell if provided
                   (when (and shell shell-args) shell-args)
                   (when command (list command)))))  ;; Add final command if provided
      ;; Debugging individual parts of the command
      formatted-command)))


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
      (abysl-term--default-exit-hook tmp-files exit-hooks)
      (if (not (eq sentinel-exit-code 0))
          (error "[abysl-term--process-sentinel] Sentinel exit code: %d" sentinel-exit-code)))))

(defun abysl-term--default-exit-hook (tmp-files exit-hooks)
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
    (dolist (file tmp-files)
      (when (file-exists-p file)
        (delete-file file)
        ))
    ;; Handle output based on exit-codes
    (abysl-term--handle-output exit-codes output)
    ;; Call user-defined hooks
    (dolist (hook exit-hooks)
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
                  (format "echo $pipestatus > %s\n" (abysl-term--convert-to-unix-path exit-code-file))
                (format "echo ${PIPESTATUS[*]} > %s\n" (abysl-term--convert-to-unix-path exit-code-file)))))
    ;; Make the script file executable
    (set-file-modes script-file #o755)
    ;; Return the list of the script file, output file, and exit code file
    (list script-file output-file exit-code-file)))

(defun abysl-term--find-shell-path (shell)
  "Find the full path of the SHELL if it's just a shell name, otherwise return the shell."
  (if (eq system-type 'windows-nt)
      (concat "/bin/" shell)
    (if (file-name-absolute-p shell)
        shell  ;; If shell is already an absolute path, return it
      (or (executable-find shell)  ;; Look up shell in PATH
          (error "Shell '%s' not found in PATH" shell)))
    )
  )

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

(defun apply-all (str replacements)
  "Apply all regex REPLACEMENTS to STR."
  (cl-reduce
   (lambda (acc pair)
     (replace-regexp-in-string (car pair) (cadr pair) acc))
   replacements
   :initial-value str))

;; (abysl-term--convert-to-unix-path "C:\\Program Files\\Git")
;; )(abysl-term--convert-to-unix-path "/c/test/path")
(defun abysl-term--convert-to-unix-path (path)
  "Convert PATH into a unix-style path."
  (if (not (eq system-type 'windows))
      path
    (let* (
           (sf (replace-regexp-in-string "\\\\" "/" path))
           (ef (replace-regexp-in-string " " "\\\\ " sf))
           (df (replace-regexp-in-string "^\\([a-zA-Z]\\):" "/\\1" ef))
           (cf (concat (downcase (substring df 0 2)) (substring df 2)))
           )
      cf
      )
    )
  )

(defun abysl-term--get-project-root-or-default ()
  "Return the root of the current Projectile project, or fallback to the parent directory of the current buffer.
If Projectile is not available, or the current workspace is invalid, return the parent directory of the current buffer.
If there is no current buffer or the buffer is not visiting a file, return the home directory."
  (let* ((project-root (if (and (fboundp 'projectile-project-root)
                                (projectile-project-p))
                           (projectile-project-root)
                         nil))
         (buffer-file-dir (if (buffer-file-name)
                              (file-name-directory (buffer-file-name))
                            nil)))
    (or project-root
        buffer-file-dir
        (expand-file-name "~"))))

(provide 'abysl-term)
;;; abysl-term.el ends here
