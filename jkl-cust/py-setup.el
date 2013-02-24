;;;; PYTHON - Pymacs and ropemacs

;;; ipython
;; requires python-mode
;;(require 'ipython)
;;(jkl/custom-set 'py-python-command-args '("--colors=Linux"))

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
  (defadvice pymacs-start-services (around activate)
    (let ((process-connection-type nil))
      ad-do-it)))

(jkl/custom-set 'py-load-pymacs-p t)
(jkl/custom-set 'py-install-directory (concat el-get-dir "python-mode"))
;;(jkl/custom-set 'py-complete-function 'py-complete-completion-at-point)
(jkl/custom-set 'py-complete-function 'auto-complete)

(require 'python-mode)
(when (and (fboundp 'py-load-pycomplete)
           (not (bound-and-true-p jkl/prefer-rope-completion)))
  (py-load-pycomplete))

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

(defvar ac-jropemacs-completions-cache nil)
(make-variable-buffer-local 'ac-jropemacs-completions-cache)

(ac-define-source pysmell
  '((candidates
     . (lambda ()
         (require 'pysmell)
         (pysmell-get-all-completions)))))

(defun ac-jropemacs-document (symbol)
  (let* ((full-prefix (py-complete-enhanced-symbol-before-point))
        (full-symbol (concat (substring full-prefix 0 (- (length ac-prefix))) symbol)))
    (py-complete-docstring-for-symbol full-symbol)))


(ac-define-source jropemacs-dot
  '((candidates . (lambda ()
                    (mapcar
                     (lambda (completion)
                       (concat ac-prefix completion))
                     (ignore-errors (rope-completions)))))
    (symbol . "p")
    (prefix . c-dot)
    (requires . 0)
    (cache)))

;; Slow as hell on OSX
(defun ac-jropemacs-setup ()
  (interactive)
  (setq ac-sources '(ac-source-jropemacs-dot ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)))

;; (set 'ac-sources (cons 'ac-source-jropemacs-dot (remq 'ac-source-pycomplete ac-sources))))
;;  (setq ac-sources (append '(ac-source-jropemacs-dot) ac-sources)))

(defun ac-pysmell-setup ()
  (interactive)
  (setq ac-sources (append ac-sources '(ac-source-pysmell))))

(defun ac-python-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'python-mode-hook 'ac-python-mode-setup)

(when (not (or (featurep 'pycomplete)
	       (eq system-type 'darwin)))
  (add-hook 'python-mode-hook 'ac-jropemacs-setup))

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
