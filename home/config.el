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

;; Functions

;; Keymaps

(map! :leader
      :desc "Colmena apply-local with current project root"
      :mode nix-mode
      "c l"
      (lambda ()
        (interactive)
        (let ((project-root (projectile-project-root)))
          (if project-root
              (run-command (format "colmena apply-local --sudo --config %sflake.nix" project-root) "onSuccess")
            (message "No project root found.")))))
