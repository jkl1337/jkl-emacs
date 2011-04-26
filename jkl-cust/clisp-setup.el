;; TODO: Add documentation hooks/standardization

(defvar jkl/clisp-dir "/usr/lib/clisp-2.49/")

(setq jkl/clisp-exe
      (if jkl/mswinp
          (concat jkl/clisp-dir "lisp.exe")
        "clisp"))

(setq clisp-hs-program
      (if jkl/mswinp
          (concat jkl/clisp-exe " -B " jkl/clisp-dir " -M " jkl/clisp-dir "lispinit.mem -ansi -q")
        (concat jkl/clisp-exe " -B " jkl/clisp-dir " -ansi -q")))

;;; SLIME setup - Slime needs to be provided by ELPA
(setq jkl/slime-lisps
     `(,(concat "clisp"
                (if (file-directory-p jkl/clisp-dir)
                    " -K full" ""))
       "c:/prp/ccl-trunk/wx86cl.exe"))
