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

;; Ensure LSP is set to use the ccls language server for C and C++
(after! lsp-mode
  (setq lsp-completion-provider t)  ;; Optional, for better completion behavior
  (setq lsp-enabled-clients '(ccls))
  ;; (setq lsp-clients-ccls-executable "ccls")  ;; Uncomment this line if you need to manually specify the ccls binary
  )

;; Enable LSP and PlatformIO for C, C++, and Arduino files
(after! platformio-mode
  (add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))  ;; Associate .ino files with arduino-mode
  (dolist (hook '(c-mode-hook c++-mode-hook arduino-mode-hook))  ;; Add hooks for C, C++, and Arduino files
    (add-hook hook (lambda ()
                     (lsp-deferred)  ;; Enable lsp-mode deferred loading
                     (platformio-conditionally-enable)))))  ;; Enable platformio-mode when appropriate

;; Functions

;; Keymaps
