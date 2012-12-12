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

;; Auto-complete with rope
(defun ac-ropemacs-candidates ()
  (mapcar (lambda (completion)
            (concat ac-prefix completion))
          (rope-completions)))

(ac-define-source jropemacs
  '((candidates . ac-ropemacs-candidates)
    (symbol . "p")))

(ac-define-source jropemacs-dot
  '((candidates . ac-ropemacs-candidates)
    (symbol . "p")
    (prefix . c-dot)
    (requires . 0)))

(defun ac-jropemacs-setup ()
  (interactive)
  (setq ac-sources (append '(ac-source-jropemacs
                             ac-source-jropemacs-dot) ac-sources)))

(defun ac-python-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'python-mode-hook 'ac-python-mode-setup)
(add-hook 'python-mode-hook
          (lambda ()
            (define-key jkl/func-map "r" 'ac-jropemacs-setup)))

;; CEDET causes this to load, and it really fouls shit up for me with python-mode
;; Leave out for now
(remove-hook 'python-mode-hook 'wisent-python-default-setup)

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\C-ci" 'rope-auto-import)
            (define-key python-mode-map "\C-c\C-d" 'rope-show-calltip)))

;; Automatically open rope project if it exists
(add-hook 'python-mode-hook
          (lambda ()
            (cond ((file-exists-p ".ropeproject")
                   (rope-open-project default-directory))
                  ((file-exists-p "../.ropeproject")
                   (rope-open-project (concat default-directory "..")))
                  )))

(pymacs-load "ropemacs" "rope-")
(jkl/custom-set 'ropemacs-codeassist-maxfixes 3)
(jkl/custom-set 'ropemacs-enable-autoimport t)
(jkl/custom-set 'ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"))

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
