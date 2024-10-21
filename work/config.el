;; User Config

(setq user-full-name "Andrea Bueide"
      user-mail-address "andrea.bueide@fiserv.com")


;; System Config

(setq doom-font (font-spec :family "JetBrainsMono NFM" :size 18)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile")
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono")
      doom-big-font (font-spec :family "JetBrainsMono NFM" :size 28))

(setq explicit-shell-file-name "bash")
(setq explicit-bash-args '("--login" "-i"))
(setq shell-file-name explicit-shell-file-name)


;; Package Config

(setq org-directory "~/work/notes")


(use-package! projectile
  :init
  (setq projectile-project-search-path '("~/work/projects" "~/work/config"))
  :config
  (projectile-add-known-project "~/.config/doom")
  (projectile-add-known-project "~/work/docs")
  (projectile-add-known-project "~/work/notes"))



(use-package! org-projectile
  :after (:and org projectile)
  :config
  (setq org-project-capture-default-backend (make-instance 'org-project-capture-projectile-backend))
  (setq org-project-capture-projects-file "~/work/notes/projects.org"))


(use-package! org
  :config
  (require 'ox-md)
  (require 'ox-publish)
  (setq org-publish-project-alist
        '(("wiki"
           :base-directory "~/work/docs"  ;; Directory where your Org wiki files are
           :base-extension "org"              ;; Only process files with the .org extension
           :publishing-directory "~/docs-md"  ;; Where the exported files will go
           :recursive t                       ;; Include subdirectories
           :publishing-function org-md-publish-to-markdown  ;; Export to Markdown
           :headline-levels 4                 ;; How many levels of headings to include
           :auto-preamble t                   ;; Automatically add preamble to exported files
           :with-toc t                        ;; Include a table of contents
           :section-numbers nil               ;; Disable section numbers
           :with-author t                   ;; Don't include author name in exported files
           :with-creator nil                  ;; Don't include Emacs and Org-mode version
           :with-date t))))                   ;; Include date in exported files

(after! lsp
  (setq lsp-disabled-clients 'lua-roblox-language-server))
