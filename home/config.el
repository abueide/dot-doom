;; User Config

(setq user-full-name "Andrea Bueide"
      user-mail-address "andrea@abueide.com"
      )

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 18)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans")
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono")
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 28))
(after! vterm (setq vterm-shell "fish"))

;; Package Config

(setq org-directory "~/notes")

(use-package! projectile
  :init
  (setq projectile-project-search-path '("~/code"))
  :config
  (projectile-add-known-project "~/.config/doom")
  (projectile-add-known-project "~/notes")
  )
