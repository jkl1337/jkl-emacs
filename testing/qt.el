;; (ede-cpp-root-project "sup-arch"
;;                       :name "sup-arch"
;;                       :file "~/git/sup-arch/sup/Makefile"
;;                       :include-path '("/")
;;                       :system-include-path '("/usr/include/c++/4.7.1"))

(jkl/test-cedet)
(progn
  (setq qt4-base-dir "/usr/include")
  (setq qt4-gui-dir (concat qt4-base-dir "/QtGui"))
  (setq qt4-core-dir (concat qt4-base-dir "/QtCore"))
  (semantic-add-system-include qt4-base-dir 'c++-mode)
  (semantic-add-system-include qt4-gui-dir 'c++-mode)
  (semantic-add-system-include qt4-core-dir 'c++-mode)
  (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-large.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))
  (semantic-c-reset-preprocessor-symbol-map))

(ede-cpp-root-project "psound"
                      :name "psound"
                      :file "~/hdd/psound/Makefile"
                      :system-include-path '("/usr/include/c++/4.7.1"
                                             "/usr/include/QtCore"
                                             "/usr/include/QtGui"))
