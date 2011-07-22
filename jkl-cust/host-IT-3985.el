(jkl/add-exec-paths "C:/Python27"
                    "C:/MinGW/msys/1.0/bin"
                    "C:/Program Files (x86)/Git/bin"
                    "C:/Program Files (x86)/Aspell/bin"
                    "C:/prp/gs/gs9.01/bin"
                    "C:/cygwin/opt/w3m/bin")

(setq find-program "/bin/find")
;;(setq find-ls-option '("-ls" . "-ilsd"))
;;(setq find-ls-option '("-exec ls -ld {} \\;" . "-ld"))

(jkl/default-set 'w3m-command "C:/cygwin/opt/w3m/bin/w3m")

(setq jkl/pkg-path "C:/prp/emacs-d/pkg/")
(setq jkl/info-path "C:/prp/emacs-d/info/")

(setq jkl/clisp-dir "C:/prp/clisp-2.49/full/")

(eval-after-load "slime"
  '(jkl/custom-set 'inferior-lisp-program "c:/prp/ccl-trunk/wx86cl.exe"))
