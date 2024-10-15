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

(use-package! abysl-term
  :load-path "lib"
  :custom
  (abysl-term-terminal "wezterm")
  (abysl-term-terminal-args (list "--always-new-process"))
  (abysl-term-hide 'onSuccess)
  :config
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
                (abysl-term-run "doom sync"
                                (lambda (exit-codes output)
                                  (message "exit hook called")
                                  (if (cl-every (lambda (x) (eq x 0)) exit-codes)
                                      (doom/restart-and-restore))))))
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
            (abysl-term-run (format "colmena apply --on %s" formatted-tags)))))
  )

(after! exec-path-from-shell (exec-path-from-shell-initialize))

;; Custom Functions
(defun projectile-insert-link (format-str)
  "Helper function to insert a link using projectile-find-file.
FORMAT-STR is used to format the link (e.g., for Org or Markdown).
If text is selected, it is used as the initial input for the prompt
with the current buffer's extension appended. The inserted link will replace
the selected text if any."
  (interactive)
  (let* ((selected-text (if (use-region-p)
                            (concat (buffer-substring-no-properties (region-beginning) (region-end))
                                    (file-name-extension (buffer-file-name) t)) ;; Add the extension
                          nil))
         (file (projectile-completing-read "Insert Link: "
                                           (projectile-current-project-files)
                                           :initial-input selected-text))
         (file-display (file-name-sans-extension (file-name-nondirectory file)))) ;; Display without extension
    (when file
      ;; If text is selected, replace it with the link
      (when (use-region-p)
        (delete-region (region-beginning) (region-end)))
      (insert (format format-str file file-display)))))

(defun projectile-insert-org-link ()
  "Insert an org link with projectile-find-file.
If text is selected, use it as the initial input with the current buffer's extension appended,
and format the link as [[file:name.ext][name]]. The inserted link replaces the selected text."
  (interactive)
  (projectile-insert-link "[[file:%s][%s]]"))

(defun projectile-insert-md-link ()
  "Insert a markdown link with projectile-find-file.
If text is selected, use it as the initial input with the current buffer's extension appended,
and format the link as [name.ext](name). The inserted link replaces the selected text."
  (interactive)
  (projectile-insert-link "[%s](%s)"))




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

(map! :leader
      :desc "List Flycheck errors"
      :mode flycheck-mode
      :nv
      "e l" #'flycheck-list-errors
      )




;; Load Profile

(let ((profile (getenv "EMACS_PROFILE")))
  (cond
   ((string= profile "work") (load! "work/config.el"))
   ((string= profile "home") (load! "home/config.el"))
   (t (load! "home/config.el")))) ;; default profile
