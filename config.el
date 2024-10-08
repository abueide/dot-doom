;; Load libs
(load! "lib/run-command.el")

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

(map!
 :desc "Run selected region as command"
 :leader
 :v "r c"
 #'run-selected-command)

(map! :leader
      :desc "List Flycheck errors"
      :mode flycheck-mode
      :nv
      "e l" #'flycheck-list-errors
      )

(map! :leader
      :desc "Sync Doom and Restart"
      :nv
      "q t" (lambda ()
              (interactive)
              (run-command "doom sync"
                           nil
                           (lambda () (doom/restart-and-restore))
                           (lambda () (message "Doom sync failed.")))))



;; Load Profile

(let ((profile (getenv "EMACS_PROFILE")))
  (cond
   ((string= profile "work") (load! "work/config.el"))
   ((string= profile "home") (load! "home/config.el"))
   (t (load! "home/config.el")))) ;; default profile
