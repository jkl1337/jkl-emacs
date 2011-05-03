
(require 'emms-setup)

(when (and nil jkl/mswinp)
  (setq pylookup-program (concat pylookup-dir "pylookup.bat"))
  (unless (file-exists-p pylookup-program)
    (let ((explicit-shell-file-name "cmd.exe"))
      (with-temp-buffer
        (write-region
         (concat "@python "
                 (convert-standard-filename
                  (concat pylookup-dir "pylookup.py")) " %*")
         nil pylookup-program)))
    t))

