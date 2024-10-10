;; User Config

(setq user-full-name "Andrea Bueide"
      user-mail-address "andrea.bueide@fiserv.com"
      )

;; System Config

(setq doom-font (font-spec :family "JetBrainsMono NFM" :size 18)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile")
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono")
      doom-big-font (font-spec :family "JetBrainsMono NFM" :size 28))

(setq explicit-shell-file-name "C:\Program Files\Git\bin\bash.exe")
(setq explicit-bash-args '("--login" "-i"))
(setq shell-file-name explicit-shell-file-name)
(add-to-list 'exec-path "C:\Program Files\Git\bin")

;; Package Config

(setq org-directory "~/work/notes")

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
