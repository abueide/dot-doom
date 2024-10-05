(load! "common/packages.el")

(let ((profile (getenv "EMACS_PROFILE")))
  (cond
   ((string= profile "work") (load! "work/packages.el"))
   ((string= profile "home") (load! "home/packages.el"))
   (t (load! "home/packages.el")))) ;; default profile 
