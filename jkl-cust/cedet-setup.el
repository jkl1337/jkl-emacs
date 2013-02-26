
(semantic-mode 1)
(jkl/cs 'semantic-new-buffer-setup-functions
        '((emacs-lisp-mode . semantic-default-elisp-setup)
          (html-mode . semantic-default-html-setup)
          (jde-mode . wisent-java-default-setup)
          (java-mode . wisent-java-default-setup)
          ;;(python-mode . wisent-python-default-setup)
          (scheme-mode . semantic-default-scheme-setup)
          (srecode-template-mode . srecode-template-setup-parser)
          (texinfo-mode . semantic-default-texi-setup)
          (makefile-automake-mode . semantic-default-make-setup)
          (makefile-gmake-mode . semantic-default-make-setup)
          (makefile-makepp-mode . semantic-default-make-setup)
          (makefile-bsdmake-mode . semantic-default-make-setup)
          (makefile-imake-mode . semantic-default-make-setup)
          (makefile-mode . semantic-default-make-setup)))

(defun jkl/test-cedet ()
  (interactive)
  (jkl/add-to-list 'semantic-new-buffer-setup-functions
                   '(c++-mode . semantic-default-c-setup))
  (jkl/add-to-list 'semantic-new-buffer-setup-functions
                   '(c-mode . semantic-default-c-setup))

  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (require 'semantic/bovine/c)
  (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("Foundation_API" . ""))
  (global-ede-mode t)
  (semantic-load-enable-excessive-code-helpers)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)
  ;;(semantic-load-enable-primary-exuberent-ctags-support)

  ;; (global-semanticdb-minor-mode 1)
  ;; (global-semantic-tag-folding-mode 1)
  ;; (require 'eassist)

  ;;(global-semantic-idle-tag-highlight-mode 1)
  )
