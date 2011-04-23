;; TODO: Add documentation hooks/standardization

(defvar jkl/clisp-dir "/usr/lib/clisp-2.49/")

(setq jkl/clisp-exe
      (if jkl/mswinp
          (concat jkl/clisp-dir "lisp.exe")
        "clisp"))

(setq clisp-hs-program
      (if jkl/mswinp
          (concat jkl/clisp-exe " -B " jkl/clisp-dir " -M " jkl/clisp-dir "lispinit.mem -ansi -q")
        (concat jkl/clisp-exe " -B " jkl/clisp-dir " -ansi -q")))

;;; SLIME setup - Slime needs to be provided by ELPA
(setq jkl/slime-lisps
     `(,(concat "clisp"
                (if (file-directory-p jkl/clisp-dir)
                    " -K full" ""))
       "c:/prp/ccl/wx86cl.exe"))

(jkl/custom-set 'inferior-lisp-program (car jkl/slime-lisps))

;;; ILISP/Inferior Lisp hook customizations
(defun jkl/setup-ilisp ()
  "Set up common variables used by ilisp."
  (interactive)
  (setq ilisp-*use-fsf-compliant-keybindings* t
        ilisp-*arglist-message-lisp-space-p* t
        ilisp-print-info-message-command t
        lisp-no-popper t)

  (add-hook 'lisp-mode-hook
            (lambda ()
              (imenu-add-to-menubar "Symbols")
              (outline-minor-mode)
              (make-local-variable 'outline-regexp)
              (setq outline-regexp "^(.*")
              (ignore-errors (semantic-default-elisp-setup))
              (set (make-local-variable lisp-indent-function)
                   'common-lisp-indent-function)))

  (add-hook 'ilisp-load-hook
            '(lambda ()
               (defkey-ilisp [(control c) (e)] 'eval-in-lisp)
               (defkey-ilisp [(control c) (\;)] 'insert-balanced-comments)
               (defkey-ilisp [(control c) (:)] 'remove-balanced-comments)
               (defkey-ilisp [(control c) (x)] 'copy-eval-dwim-lisp)
               ;; Set the inferior Lisp directory to the directory of
               ;; the buffer that spawned it on the first prompt.
               (add-hook 'ilisp-init-hook
                         '(lambda ()
                            (default-directory-lisp ilisp-last-buffer)))))
  (require 'completer)
  (require 'ilisp)
  ;; Fix clisp interaction buffer (Windows)
  ;; (modify-coding-system-alist 'process "lisp" 'unix)
  ;; All the *.d and *.lisp sources are in UTF-8 encoding.
  (modify-coding-system-alist 'file "\\.\\(d\\|lisp\\)\\'" 'utf-8))
