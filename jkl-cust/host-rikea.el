
(setq user-emacs-directory "/home/jluebs/emacs/")
(setq jkl/pkg-path (concat user-emacs-directory "pkg/"))
(setq jkl/info-path (concat user-emacs-directory "info/"))

(eval-after-load "slime"
  '(jkl/custom-set 'inferior-lisp-program "sbcl"))

(eval-after-load "jde"
  '(progn
     (jkl/custom-set 'jde-jdk-registry
                     '(("1.6.0" . "/opt/java6")))))

