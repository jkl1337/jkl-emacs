;;;; PYTHON - Pymacs and ropemacs

;;; ipython
;; requires python-mode
;;(require 'ipython)
;;(jkl/cs 'py-python-command-args '("--colors=Linux"))

(defmacro jkl/define-py-shell (name desc)
  "Define a custom python interpreter for shell `name'"
  `(defun ,(intern name) (&optional argprompt dedicated switch)
     ,(concat "Start a " desc " interpreter;

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs.")
     (interactive "P")
     (py-shell argprompt dedicated ,name switch)))

(jkl/define-py-shell "ipython2" "IPython 2.x")

;; This probably doesn't help much. ropemacs is still terribly slow on Darwin.
(when (eq system-type 'darwin)
  (defadvice pymacs-start-services (around pymacs-start-services-connection activate)
    (let ((process-connection-type nil))
      ad-do-it)))

(jkl/cs 'py-load-pymacs-p t)
(jkl/cs 'py-install-directory (concat el-get-dir "python-mode"))
;;(jkl/cs 'py-complete-function 'py-complete-completion-at-point)

(jkl/cs 'py-complete-function 'auto-complete) ;; FRAGILE; python-mode is a POS and will load py-load-pycomplete
(require 'python-mode)
(when (and (fboundp 'py-load-pycomplete)
           (eq system-type 'darwin))
  (py-load-pycomplete))

(when (file-executable-p "/usr/bin/python2")
  (setq pymacs-python-command "python2")
  (jkl/cs 'py-shell-name "python2")
  (jkl/cs 'py-python-command "python2")
  (jkl/cs 'py-default-interpreter "python2"))

;; Auto-complete with rope
(defun ac-ropemacs-document (item)
  (car item))

(defun ac-ropemacs-candidates ()
  (save-match-data
    (loop for (name doc type) in (ignore-errors (rope-extended-completions))
          unless (string-match ":" name)
          collect (list (concat ac-prefix name) doc))))

(ac-define-source jropemacs-dot
  '((candidates . ac-ropemacs-candidates)
    (document . ac-ropemacs-document)
    (symbol . "p")
    (prefix . c-dot)
    (requires . 0)
    (cache)))

;; (set 'ac-sources (cons 'ac-source-jropemacs-dot (remq 'ac-source-pycomplete ac-sources))))

(defun ac-python-mode-setup ()
  (setq ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (unless (or (eq system-type 'darwin)
              (bound-and-true-p jkl/inhibit-rope-completion))
    (setq ac-sources (cons 'ac-source-jropemacs-dot ac-sources)))
  (setq ac-sources (cons 'ac-source-yasnippet ac-sources)))

;;(add-hook 'python-mode-hook 'ac-python-mode-setup)

;; Automatically open rope project if it exists
(defun jkl-setup-rope ()
  (cond ((file-exists-p ".ropeproject")
         (rope-open-project default-directory))
        ((file-exists-p "../.ropeproject")
         (rope-open-project (concat default-directory "..")))))

;; (add-hook 'python-mode-hook 'jkl-setup-rope)

;; CEDET causes this to load, and it really fouls shit up for me with python-mode
;; Leave out for now
(remove-hook 'python-mode-hook 'wisent-python-default-setup)

(define-key python-mode-map "\C-ci" 'rope-auto-import)
(define-key python-mode-map "\C-c\C-d" 'rope-show-calltip)

(pymacs-load "ropemacs" "rope-")
(jkl/cs 'ropemacs-guess-project t
        'ropemacs-codeassist-maxfixes 3
        'ropemacs-enable-autoimport t
        'ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"))

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
