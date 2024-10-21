;; User Config

(setq user-full-name "Andrea Bueide"
      user-mail-address "andrea@abueide.com")


(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 18)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans")
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono")
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 28))

(setq org-directory "~/notes")
(setq vterm-shell "fish")

;; Package Config

(use-package! projectile
  :init
  (setq projectile-project-search-path '("~/code"))
  :config
  (projectile-add-known-project "~/.config/doom")
  (projectile-add-known-project "~/notes"))

;; Tree-sitter configuration
(use-package! tree-sitter
  :custom
  (treesit-extra-load-path '("~/.config/emacs/tree-sitter")))

;; LSP Mode configuration
(after! lsp-mode
  (setq lsp-completion-provider t)  ;; Optional, for better completion behavior
  (setq lsp-enabled-clients '(ccls))
  ;; Register Glistix as the LSP server for Gleam
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("glistix" "lsp"))
    :major-modes '(gleam-ts-mode)
    :server-id 'glistix-lsp)))

;; Gleam-ts-mode configuration
(use-package! gleam-ts-mode
  :mode "\\.gleam\\'" ;; Automatically use gleam-ts-mode for .gleam files
  :hook (gleam-ts-mode . lsp) ;; Automatically enable lsp-mode in gleam-ts-mode
  :config
  ;; Set up formatter for Apheleia
  (after! apheleia
    (setf (alist-get 'gleam-ts-mode apheleia-mode-alist) 'gleam)
    (setf (alist-get 'gleam apheleia-formatters) '("glistix" "format" "--stdin"))))

(use-package! parinfer-rust-mode
  :custom
  (parinfer-rust-auto-download 0) ;(parinfer-rust-library "~/.config/emacs/parinfer-rust/parinfer-rust.so")
  :hook ((emacs-lisp-mode
          clojure-mode
          scheme-mode
          lisp-mode
          racket-mode
          fennel-mode
          hy-mode
          . parinfer-rust-mode))
  :init
  (map!  :map parinfer-rust-mode-map
         :localleader
         "p" #'parinfer-rust-switch-mode
         "P" #'parinfer-rust-toggle-disable))


(use-package! gptel
  :custom
  (map!
   :leader
   "c h c" 'gptel
   "c h s" 'gptel-send
   "c h m" 'gptel-menu
   "c h a" 'gptel-add
   "c h f" 'gptel-add-file))



;; Functions

(defun sync-and-restart () (interactive) (abysl-term-run "doom sync" :onSuccess #'doom/restart-and-restore))
(defun colmena-apply-local () (interactive)
       (abysl-term-run (format "colmena apply-local --sudo --config %sflake.nix --impure" (projectile-project-root))))
(defun colmena-apply-on () (interactive)
       (let* ((tags (read-string "Enter tags (space or comma separated): "))
              (formatted-tags (mapconcat 'identity (split-string tags "[ ,]+" t) ","))
              (abysl-term-run (format "colmena apply --on %s --config %sflake.nix --impure" formatted-tags (projectile-project-root))))))




;; Keymaps
(after! platformio-mode
  (add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))
  (dolist (hook '(c-mode-hook c++-mode-hook arduino-mode-hook))
          (add-hook hook (lambda () (lsp-deferred) (platformio-conditionally-enable)))))

(after! abysl-term
  ;; Sync Doom and Restart
  (map! :leader
        :desc "sync and restart"
        "q t" #'sync-and-restart)
  
  ;; Colmena commands for nix-mode
  (map! :localleader
        :mode nix-mode
        :prefix ("c" . "colmena")
        :desc "[c]olmena apply [l]ocal"
        "l" #'colmena-apply-local
        :desc "[c]olmena apply --[o]n"
        "o" #'colmena-apply-on))
