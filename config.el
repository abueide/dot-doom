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
  (abysl-term-terminal "wezterm")
  (abysl-term-terminal-args (list "--always-new-process"))
  (abysl-term-hide 'onSuccess)
  :config
  (map! :leader
        :prefix ("r" . "run")
        :desc "Run selected text"
        "c" #'abysl-term-run-selected)
  (map! :leader
        :prefix ("r" . "run")
        :desc "Run previous command"
        "p" #'abysl-term-run-previous)
  (map! :leader
        :prefix ("o" . "open")
        :desc "[o]pen abysl-term [s]hell"
        "s" #'abysl-term-open-shell)
  (map! :leader
        :desc "Sync Doom and Restart"
        :nv
        "q t" (lambda ()
                (interactive)
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
