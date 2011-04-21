(setq user-emacs-directory "C:/prp/emacs-d/")

(jkl/add-exec-paths "C:/prp/Python27"
                    "C:/MingW/msys/1.0/bin"
                    "C:/prp/Git/bin"
                    "C:/prp/Mercurial"
                    "C:/prp/Aspell/bin"
                    "C:/prp/gs/gs9.01/bin"
                    "C:/cygwin/opt/w3m/bin")

(setq find-program "/bin/find")
;;(setq find-ls-option '("-ls" . "-ilsd"))
;;(setq find-ls-option '("-exec ls -ld {} \\;" . "-ld"))

(jkl/default-set 'w3m-command "C:/cygwin/opt/w3m/bin/w3m")

(setq jkl/pkg-path "C:/prp/emacs-d/pkg/")
(setq jkl/info-path "C:/prp/emacs-d/info/")

(setq jkl/clisp-dir "C:/prp/clisp-2.49/full/")

(defconst jkl/harris-sc system-name)

(jkl/load-script "harris.el")
