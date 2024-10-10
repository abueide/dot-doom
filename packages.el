(package! super-save)
(package! key-chord)
(package! key-seq)
(package! space-chord)
(package! treemacs)
(package! treemacs-icons-dired)
;;(package! org-super-agenda)
(package! exec-path-from-shell)
;;(package! org-projectile)


(let ((profile (getenv "EMACS_PROFILE")))
  (cond
   ((string= profile "work") (load! "work/packages.el"))
   ((string= profile "home") (load! "home/packages.el"))
   (t (load! "home/packages.el")))) ;; default profile 
