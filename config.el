
(load! "common/config.el")
(load! "lib/run-command.el")

(let ((profile (getenv "EMACS_PROFILE")))
  (cond
   ((string= profile "work") (load! "work/config.el"))
   ((string= profile "home") (load! "home/config.el"))
   (t (load! "home/config.el")))) ;; default profile
