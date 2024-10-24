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
(setq backward-delete-char-untabify-method 'untabify)

;; Custom Functions

(defun get-selected-text ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    nil))

(defun file-display-name (file)
  (let* ((removed-dir (file-name-nondirectory file))
         (removed-ext (file-name-sans-extension removed-dir))
         (replace-with-spaces (replace-regexp-in-string "-" " " removed-ext)))
    replace-with-spaces))


(defun display-name-to-file (str)
  (let* ((ext-added (add-ext str))
         (slugify (replace-regexp-in-string " " "-" ext-added)))
    slugify))

(defun add-ext (str)
  (if (and str (string-match-p ".*\.(.+)$" str))
      (concat str (file-name-extension buffer-file-name t))
    str))

(defun delete-selected-text ()
  (when (use-region-p)
    (delete-region (region-beginning) (region-end))))

(defun projectile-insert-link (formatter)
  (let* (
         (format-str "")
         (selected-text (get-selected-text))
         (file
          (projectile-completing-read
           "Insert Link"
           (projectile-current-project-files)
           :initial-input
           (display-name-to-file selected-text)))
         (display-text (file-display-name file)))
    (when file
      (delete-selected-text)
      (insert (funcall formatter display-text file)))))

(defun md-link-format (display-text link)
  (format "[%s](%s)" display-text link))

(defun org-link-format (display-text link)
  (format "[[%s][%s]]" link display-text))

(defun wiki-link-format (display-text link)
  (format "[[%s|%s]]" link display-text))

(defun projectile-insert-org-link ()
  "Insert an org link with projectile-find-file.
If text is selected, use it as the initial input with the current buffer's extension appended,
and format the link as [[file:name.ext][name]]. The inserted link replaces the selected text."
  (interactive)
  (projectile-insert-link 'org-link-format))

(defun projectile-insert-md-link ()
  "Insert a markdown link with projectile-find-file.
If text is selected, use it as the initial input with the current buffer's extension appended,
and format the link as [name.ext](name). The inserted link replaces the selected text."
  (interactive)
  (projectile-insert-link 'md-link-format))

(defun projectile-insert-wiki-link ()
  "Insert a wiki link with projectile-find-file."
  (interactive)
  (projectile-insert-link 'wiki-link-format))

;; Package Config

(use-package! lsp-mode
  :custom
  (lsp-completion-provider :capf)
  (lsp-enable-completion-at-point t)
  (lsp-completion-enable t))

(use-package! org-roam
  :after org
  :config
  (org-roam-db-autosync-mode))


(use-package! super-save
  :custom
  (super-save-auto-save-when-idle t)
  (auto-save-default nil)
  :config
  (super-save-mode +1)
  (add-to-list 'super-save-hook-triggers 'find-file-hook))


(use-package! parinfer-rust-mode
  :hook ((emacs-lisp-mode
          clojure-mode
          scheme-mode
          lisp-mode
          racket-mode
          fennel-mode
          hy-mode) . parinfer-rust-mode)
  :init
  :config)


(use-package! abysl-term
  :load-path "lib"
  :custom
  (abysl-term-terminal "wezterm")
  (abysl-term-terminal-args (list "--always-new-process"))
  (abysl-term-hide 'onSuccess))

(use-package! simple-httpd
  :defer t
  :custom
  (httpd-port 7070))


(use-package! markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode
  (("\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "pandoc -f gfm -t html5")
  (markdown-enable-wiki-links t)
  (markdown-split-window-direction 'right))


;; Keymaps

(use-package! key-chord
  :init
  (key-chord-mode t)
  :config
  (key-chord-define evil-insert-state-map "fd" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "FD" 'evil-normal-state))


(map! :map 'evil-normal-state-map
      :leader
      "j l" 'evil-join
      "p v" '+treemacs/toggle)


(after! org-mode
  (map!
   :mode org-mode
   :localleader
   "l p" 'projectile-insert-org-link))

(after! markdown-mode
  (map!
   :mode markdown-mode
   :localleader
   "p c" 'markdown-preview
   "p l" 'markdown-live-preview-mode)

  (map!
   :mode markdown-mode
   :localleader
   "l p" 'projectile-insert-md-link))


(map! :map 'evil-normal-state-map
      "J" 'centaur-tabs-backward
      "K" 'centaur-tabs-forward)


(after! flycheck-mode
  (map! :leader
        :desc "List Flycheck errors"
        :mode flycheck-mode
        :nv
        "e l" #'flycheck-list-errors))


(after! abysl-term
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
                (let ((cmd (if (eq system-type 'windows-nt)
                               "c:/Users/FAFI3L8/.config/emacs/bin/doom sync"
                             "doom sync")))
                  (abysl-term-run cmd
                                  (lambda (exit-codes output)
                                    (if (cl-every (lambda (x) (eq x 0)) exit-codes)
                                        (doom/restart-and-restore)))))))
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
            (abysl-term-run (format "colmena apply --on %s" formatted-tags))))))
;; Load Profile

(let ((profile (getenv "EMACS_PROFILE")))
  (cond
   ((string= profile "work") (load! "work/config.el"))
   ((string= profile "home") (load! "home/config.el"))
   (t (load! "home/config.el")))) ;; default profile
