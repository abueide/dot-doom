;; Load libs
(add-to-list 'load-path (expand-file-name "lib" doom-user-dir))
;; Startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq shell-file-name (executable-find "bash"))

;; User Config

(setq doom-theme 'doom-dracula)
(setq display-line-numbers-type 'relative)
(setq warning-minimum-level :error)
(setq org-return-follows-link t)

;; Package Config
;;
;;
(use-package! org-roam
  :after org
  :config
  (org-roam-db-autosync-mode)
  )

(use-package! super-save
  :init
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil)
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  )

(use-package! abysl-term
  :load-path "lib"
  :custom
  ;; Set default terminal
  (abysl-term-terminal "wezterm")
  ;; Set default terminal args
  (abysl-term-terminal-args (list "--always-new-process" "--workspace" "AbyslWorkspace"))
  ;; Set default shell
  (abysl-term-shell "fish")
  ;; When to hide the buffer that opens containing the command's output after it completes
  (abysl-term-hide 'onSuccess)
  ;; Extend the `abysl-term-args-alist` to add support for arbitrary terminal emulators.
  ;; Please consider making a PR after testing :)
  (abysl-term-args-alist
   (append abysl-term-args-alist
           ;; Flag to set the working directory.
           ;; By default uses projectile-project-root
           ;; or if unavailable current buffer's directory
           ;; or if unavailable user's home directory
           ;; The terminal name tag must match the terminal's binary name in the PATH
           '((wezterm . ((cd . "--work-dir")
                         ;; Subcommand to launch an executable within the terminal (if any) Starting command (if any)
                         (start . "start")
                         ;; Use '--' separator to separate terminal args from program args.
                         (use-argument-separator . t))))))
  :config
  (map! :leader
        :prefix ("r" . "run")
        :desc "Run selected text"
        "c" #'abysl-term-run-selected)
  (map! :leader
        :prefix ("r" . "run")
        :desc "Run previous command"
        "p" #'abysl-term-run-previous)
  ;; Opens your default shell in your default terminal
  (map! :leader
        :prefix ("o" . "open")
        :desc "[o]pen abysl-term [s]hell"
        "s" #'abysl-term-open-shell)
  (map! :leader
        :desc "Sync Doom and Restart"
        :nv
        "q t" (lambda ()
                (interactive)
                ;; Example of adding a custom exit hook to an invocation of abysl-term-run
                ;; exit-codes contains an exit code for each command in your pipe
                ;; Pretend there is a command called simulate-error which returns an argument as an error code
                ;; simulate-error 0 | simulate-error 44 | simulate-error 25
                ;; would make exit codes = (list 0 44 25) when the command completes
                ;; cl-every ensures we only restart if there are no errors so we can see them if they happen
                (abysl-term-run "doom sync"
                                (lambda (exit-codes output)
                                  (message "exit hook called")
                                  (if (cl-every (lambda (x) (eq x 0)) exit-codes)
                                      (doom/restart-and-restore))))))
  (map! :leader
        :mode nix-mode
        :prefix ("c" . "colmena")
        :desc "Colmena apply-local --sudo" "l"
        (lambda ()
          (interactive)
          (abysl-term-run "colmena apply-local --sudo"))

        :desc "Colmena apply with tags" "a"
        (lambda ()
          (interactive)
          (let* ((tags (read-string "Enter tags (space or comma separated): "))
                 (formatted-tags (mapconcat 'identity (split-string tags "[ ,]+" t) ",")))
            (abysl-term-run (format "colmena apply --on %s" formatted-tags)))))
  )

(after! exec-path-from-shell (exec-path-from-shell-initialize))

;; Custom Functions
(defun projectile-insert-org-link
    ()
  "Insert org link with projectile-find-file"
  (interactive)
  (let (
        (file (projectile-completing-read
               "Insert Link: "
               (projectile-current-project-files)
               )
              )
        )
    (when file
      (insert (format "[[file:%s][%s]]"
                      file
                      (file-name-sans-extension (file-name-nondirectory file))
                      )
              )
      )
    )
  )

(defun projectile-insert-md-link
    ()
  "Insert org link with projectile-find-file"
  (interactive)
  (let (
        (file (projectile-completing-read
               "Insert Link: "
               (projectile-current-project-files)
               )
              )
        )
    (when file
      (insert (format "[%s](%s)"
                      (file-name-sans-extension (file-name-nondirectory file))
                      file
                      )
              )
      )
    )
  )

;; Keymaps

(use-package! key-chord
  :init
  (key-chord-mode t)
  :config
  (key-chord-define evil-insert-state-map "fd" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "FD" 'evil-normal-state)
  )

(map! :map 'evil-normal-state-map
      :leader
      "j l" 'evil-join
      "p v" '+treemacs/toggle
      )

(map!
 :mode org-mode
 :localleader
 "l p" 'projectile-insert-org-link
 )

(map!
 :mode markdown-mode
 :localleader
 "l p" 'projectile-insert-md-link
 )

(map! :map 'evil-normal-state-map
      "J" 'centaur-tabs-backward
      "K" 'centaur-tabs-forward
      )

(map! :leader
      :desc "List Flycheck errors"
      :mode flycheck-mode
      :nv
      "e l" #'flycheck-list-errors
      )




;; Load Profile

(let ((profile (getenv "EMACS_PROFILE")))
  (cond
   ((string= profile "work") (load! "work/config.el"))
   ((string= profile "home") (load! "home/config.el"))
   (t (load! "home/config.el")))) ;; default profile
