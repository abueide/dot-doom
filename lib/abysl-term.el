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

(defcustom abysl-term-terminal 'wezterm
  "The terminal to use: 'vterm' or 'wezterm'."
  :type '(choice (const :tag "vterm" vterm)
          (const :tag "WezTerm" wezterm))
  :group 'abysl-term)

(defcustom abysl-term-terminal-args '("start" "--always-new-process" "--")
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

(defvar abysl-term--last-command nil
  "The last command run by `abysl-term-run'.")

(defun abysl-term--get-command-str (command)
  "Convert COMMAND to a string if it is a list."
  (message "[abysl-term--get-command-str] Command: %s" command)
  (if (listp command)
      (mapconcat 'identity command " ")
    command))

(defun abysl-term--run-vterm (command-str shell shell-args buffer-name)
  "Run COMMAND-STR in vterm using SHELL and SHELL-ARGS in BUFFER-NAME."
  (require 'vterm)
  (message "[abysl-term--run-vterm] Command: %s, Shell: %s, Shell Args: %s, Buffer: %s" command-str shell shell-args buffer-name)
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (vterm-mode)
      (vterm-send-string (concat shell " " (mapconcat 'identity shell-args " ") " " command-str))
      (vterm-send-return))
    (abysl-term--handle-buffer-display buffer)))

(defun abysl-term--run-wezterm (command-str terminal-args shell shell-args buffer-name)
  "Run COMMAND-STR in WezTerm using TERMINAL-ARGS, SHELL, SHELL-ARGS in BUFFER-NAME."
  (let ((wezterm-command (mapcar (lambda (arg) (if (symbolp arg) (symbol-name arg) arg))
                                 (append terminal-args (list shell) shell-args (list command-str)))))
    (message "[abysl-term--run-wezterm] WezTerm Command: %s" wezterm-command)
    (apply 'start-process "*wezterm*" buffer-name "wezterm" wezterm-command))
  (when (eq abysl-term-hide 'never)
    (display-buffer buffer-name)))

(defun abysl-term--handle-buffer-display (buffer)
  "Handle the display behavior of BUFFER based on `abysl-term-hide`."
  (message "[abysl-term--handle-buffer-display] Buffer: %s, Hide setting: %s" buffer abysl-term-hide)
  (cond
   ((eq abysl-term-hide 'onSuccess)
    (set-process-sentinel (get-buffer-process buffer)
                          (lambda (_proc event)
                            (message "[abysl-term--handle-buffer-display] Process event: %s" event)
                            (when (string-match-p "exited abnormally" event)
                              (display-buffer buffer)))))
   ((eq abysl-term-hide 'always)
    (kill-buffer buffer))
   ((eq abysl-term-hide 'never)
    (display-buffer buffer))))

;; Function for running a command
(defun abysl-term-run (command &optional terminal terminal-args shell shell-args)
  "Run COMMAND in a terminal, with optional overrides for TERMINAL, TERMINAL-ARGS, SHELL, and SHELL-ARGS."
  (message "[abysl-term-run] Command Args: %s, Terminal: %s, Terminal Args: %s, Shell: %s, Shell Args: %s" command terminal terminal-args shell shell-args)
  (let* ((term (or terminal abysl-term-terminal))
         (term-args (or terminal-args abysl-term-terminal-args))
         (sh (or shell abysl-term-shell))
         (sh-args (or shell-args abysl-term-shell-args))
         (buffer-name "*abysl-term-output*")
         (command-str (abysl-term--get-command-str command)))
    (message "[abysl-term-run] Command Vars - Command: %s, Terminal: %s, Terminal Args: %s, Shell: %s, Shell Args: %s" command terminal terminal-args shell shell-args)
    (setq abysl-term--last-command (list command term term-args sh sh-args))
    (cond
     ((eq term 'vterm)
      (abysl-term--run-vterm command-str sh sh-args buffer-name))
     ((eq term 'wezterm)
      (abysl-term--run-wezterm command-str term-args sh sh-args buffer-name)))))

;; Function for running a command as a list of arguments
(defun abysl-term-run-args (args &optional terminal terminal-args shell shell-args)
  "Run a command as a list of ARGS, with optional overrides for TERMINAL, TERMINAL-ARGS, SHELL, and SHELL-ARGS."
  (message "[abysl-term-run-args] Args: %s" args)
  (abysl-term-run (mapconcat 'identity args " ") terminal terminal-args shell shell-args))

;; Run the currently selected text as a command
(defun abysl-term-run-selected ()
  "Run the currently selected text as a command."
  (interactive)
  (if (use-region-p)
      (let ((command (buffer-substring-no-properties (region-beginning) (region-end))))
        (message "[abysl-term-run-selected] Selected command: %s" command)
        (abysl-term-run command))
    (message "No region selected")))

;; Run the previous command
(defun abysl-term-run-previous ()
  "Run the previous command with the same arguments."
  (interactive)
  (if abysl-term--last-command
      (progn
        (message "[abysl-term-run-previous] Running previous command: %s" abysl-term--last-command)
        (apply 'abysl-term-run abysl-term--last-command))
    (message "No previous command to run")))

(provide 'abysl-term)
;;; abysl-term.el ends here
;;; echo test
;;; colmena fdh
;;; vim
