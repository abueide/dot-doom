;; Startup

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; User Config

(setq user-full-name "Andrea Bueide"
      user-mail-address "andrea.bueide@fiserv.com"
      )

(setq doom-theme 'doom-dracula)
(setq display-line-numbers-type 'relative)
(setq warning-minimum-level :error)

;; System Config

(if (eq system-type 'windows-nt)
    (setq doom-font (font-spec :family "JetBrainsMono NFM" :size 18)
          doom-variable-pitch-font (font-spec :family "Iosevka Etoile")
          doom-symbol-font (font-spec :family "Symbols Nerd Font Mono")
          doom-big-font (font-spec :family "JetBrainsMono NFM" :size 28))
  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 18)
        doom-variable-pitch-font (font-spec :family "Iosevka Etoile")
        doom-symbol-font (font-spec :family "Symbols Nerd Font Mono")
        doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 28))
  (after! vterm (setq vterm-shell "fish"))
  )

;; Package Config

(use-package! org
  :init
  (setq org-directory "~/work/notes")
  (setq org-return-follows-link  t)
  )

(use-package! org-roam
  :after org
  :config
  (org-roam-db-autosync-mode)
  )

(use-package! projectile
  :init
  (setq projectile-project-search-path '("~/work/projects" "~/work/config"))
  :config
  (projectile-add-known-project "~/.config/doom")
  (projectile-add-known-project "~/work/docs")
  (projectile-add-known-project "~/work/notes")
  )

(use-package! org-projectile
  :after (:and org projectile)
  :config
  (setq org-project-capture-default-backend (make-instance 'org-project-capture-projectile-backend))
  (setq org-project-capture-projects-file "~/work/notes/projects.org")
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
;;(defun my/projectile-insert-org-link
;;    ()
;; "Insert org link with projectile-find-file"
;;(interactive)
;;  (let ((file (projectile-find-file)))
;;    ((when file (insert (format "[[file:%s]]" file (file-name-nondirectoryfile))))))


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
