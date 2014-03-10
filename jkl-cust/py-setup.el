;;;; PYTHON

;; CEDET causes this to load, and it really fouls shit up for me with python-mode
;; Leave out for now
(remove-hook 'python-mode-hook 'wisent-python-default-setup)

;; cython
(require 'cython-mode)

;;;; Documentation
;;; PyLookup
(let ((pylookup-dir (concat user-emacs-directory "pylookup/")))
  (unless (file-directory-p pylookup-dir)
    (make-directory pylookup-dir))
  (setq pylookup-db-file (concat pylookup-dir "pylookup.db")))

;; (setq pylookup-program (concat pylookup-dir 
;;                                (if jkl/mswinp "pylookup.exe"
;;                                  "pylookup.py")))

;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)
;; (autoload 'pylookup-update "pylookup"
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)
(define-key global-map [?\C-h ?\C-l] 'pylookup-lookup)

;;; Info look
;; Fix for new style Sphinx generated python info pages
(require 'info-look)
(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
 '(("(python)Index" nil "")))
