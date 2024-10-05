;; Startup

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; User Config

(setq doom-theme 'doom-dracula)
(setq display-line-numbers-type 'relative)
(setq warning-minimum-level :error)


;; Package Config

(after! org (org-return-follows-link t)

(use-package! org-roam
  :after org
  :config
  (org-roam-db-autosync-mode)
  )

(use-package! super-save
  :ensure t
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

(map! :map 'evil-normal-state-map
      :mode org-mode
      :leader
      "i l" 'projectile-insert-org-link
      )
(map! :map 'evil-normal-state-map
      :mode markdown-mode
      :leader
      "i l" 'projectile-insert-md-link
      )

(map! :map 'evil-normal-state-map
      "J" 'centaur-tabs-backward
      "K" 'centaur-tabs-forward
      )
