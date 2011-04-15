(defun try-this-2 (name dir)
  (mapcar
   '(lambda (path)
      (concat path "/" name)
      )
   
   (let ((paths))
     (setq paths
           (with-temp-buffer
             (set (make-local-variable 'default-directory) dir)
             (call-process "make" nil (current-buffer) nil 
                           "-f/home/jluebs/f3b/pdir-wb.mk")
             (buffer-string)))
     (split-string paths))))

(defun try-this (dir)
     (let ((paths)
           (err))
     (setq paths
           (with-temp-buffer
             (setq default-directory dir)
             (setq err (call-process "make" nil (current-buffer) nil 
                                     "-f/home/jluebs/f3b/pdir-wb.mk"))
             (if err ""
               (buffer-string))))
     (split-string paths)))

(defun get-include-dirs (top-dir)
  "Get a list of all unique directories below top-dir that
contain header (.h) files"

  (setq top-dir (file-name-as-directory (expand-file-name top-dir)))
  (split-string
   (with-output-to-string
     (with-current-buffer standard-output
       (setq default-directory top-dir)
       (process-file-shell-command
        (format "find . -path '*/.svn' -prune -o -iname '*.h*' -printf \"%%h\\n\" | cut -c2- | sort -u"
                top-dir)
        nil (current-buffer))))))

(setq jkl/harris-addl-includes '("/Waveform/p25/src/lib/etherstack"))
(setq jkl/harris-sys-includes '("/opt/qnx640/target/qnx6/usr/include"
                                "/opt/qnx640/target/qnx6/usr/include/c++/4.2.4"))

(ede-cpp-root-project
 "nbhh_7_0_9"
 :file "/home/jluebs/f3b/nbhh.r7.0.9/falcon3/Makefile"
 :include-path (nconc
                (get-include-dirs "/home/jluebs/f3b/nbhh.r7.0.9/falcon3")
                jkl/harris-addl-includes)
 :system-include-path jkl/harris-sys-includes
 :spp-table '( ("CONFIG_SLOT" . "default")
               ("CONFIG_TARGET_PLATFORM" . "hh")))

(ede-cpp-root-project
 "nbhh_7_0_8"
 :file "/home/jluebs/f3b/nbhh.r7.0.8/falcon3/Makefile"
 :include-path (nconc
                (get-include-dirs "/home/jluebs/f3b/nbhh.r7.0.8/falcon3")
                jkl/harris-addl-includes)
 :system-include-path jkl/harris-sys-includes
 :spp-table '( ("CONFIG_SLOT" . "default")
               ("CONFIG_TARGET_PLATFORM" . "hh")))

(ede-cpp-root-project
 "hh_sunrise"
 :file "/home/jluebs/f3b/hh.sunrise/falcon3/Makefile"
 :include-path (nconc
                (get-include-dirs "/home/jluebs/f3b/hh.sunrise/falcon3")
                jkl/harris-addl-includes)
 :system-include-path  jkl/harris-sys-includes
 :spp-table '( ("CONFIG_SLOT" . "default")
               ("CONFIG_TARGET_PLATFORM" . "hh")))
