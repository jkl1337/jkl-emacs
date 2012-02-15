;;;; PYTHON - Pymacs and ropemacs

;;; ipython
;; requires python-mode
;;(require 'ipython)
;;(jkl/custom-set 'py-python-command-args '("--colors=Linux"))

(require 'python-mode)
(when (file-executable-p "/usr/bin/python2")
  (setq pymacs-python-command "python2")
  (jkl/custom-set 'py-shell-name "python2")
  (jkl/custom-set 'py-python-command "python2")
  (jkl/custom-set 'py-default-interpreter "python2"))

;; CEDET causes this to load, and it really fouls shit up for me with python-mode
;; Leave out for now
(remove-hook 'python-mode-hook 'wisent-python-default-setup)

(pymacs-load "ropemacs" "rope-")
(jkl/custom-set 'ropemacs-enable-autoimport t)

;; cython
(require 'cython-mode)


;;;; Documentation
;;; PyLookup
;; FIXME: I don't know a better way to do this, but I think there is
;; (setq pylookup-dir (file-name-as-directory (concat jkl/pkg-path "pylookup")))
;; (setq pylookup-db-file (concat pylookup-dir "pylookup.db"))

;; (setq pylookup-program (concat pylookup-dir 
;;                                (if jkl/mswinp "pylookup.exe"
;;                                  "pylookup.py")))

;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)
;; (autoload 'pylookup-update "pylookup"
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)
;; (define-key global-map [?\C-h ?\C-l] 'pylookup-lookup)


;;; Info look
;; Fix for new style Sphinx generated python info pages
(require 'info-look)
(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
 '(("(python)Index" nil "")))
