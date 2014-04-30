
(add-hook 'hs-minor-mode-hook
          '(lambda  ()
             (define-key hs-minor-mode-map (kbd "C-+") 'hs-toggle-hiding)))

;; Lisp in general (SLIME)
(eval-after-load 'slime
  '(define-key slime-mode-map (kbd "C-c p")
     'slime-pprint-eval-last-expression))

;; LUA Mode settings
;; lua-mode install is in top-level
(add-hook 'lua-mode-hook
          (lambda ()
            (interactive)
            (hs-minor-mode)
            (lua-block-mode t)))

;;; Documentation helpers
(when (fboundp 'w3m-browse-url)
  (jkl/cs
   'browse-url-browser-function
   (nconc
    (unless jkl/mswinp
      ;; Non-windows platform only
      '(("file:.*/usr/local/share/gtk-doc/html" . w3m-browse-url)
        ("file:.*/usr/share/gtk-doc/html" . w3m-browse-url)
        ("file:.*/usr/share/devhelp/books" . w3m-browse-url)
        ("file:.*/usr/share/doc" . w3m-browse-url)
        ("\\(file://\\)?/.*/hyperspec" . w3m-browse-url)))
    ;; Common set
    `(("file:.*/pylookup/python-2" . w3m-browse-url)
      ("http://.*docs.python.org/". w3m-browse-url)
      ("http://www\\.clisp\\.org/impnotes" . w3m-browse-url)
      ("http://clisp\\.cons\\.org/impnotes" . w3m-browse-url)
      ("http://www\\.lispworks\\.com/reference/HyperSpec" . w3m-browse-url)
      ("." . ,(if jkl/mswinp 'browse-url-default-windows-browser
                'browse-url-default-browser))))))


(autoload 'gtk-lookup-symbol "gtk-look" nil t)
(unless jkl/mswinp
  (jkl/cs
   'gtk-lookup-devhelp-indices
   '("/usr/share/gtk-doc/html/*/*.devhelp*"
     "/usr/local/share/gtk-doc/html/*/*.devhelp*"
     "/usr/share/devhelp/books/*/*.devhelp*"))
  (define-key global-map [?\C-h ?\C-j] 'gtk-lookup-symbol))

;;;; C styles
(jkl/cs 'c-default-style
        '((java-mode . "java")
          (awk-mode . "awk")
          (other . "stroustrup")))

;; Wrapper function needed for Emacs 21 and XEmacs (Emacs 22 offers the more
;; elegant solution of composing a list of lineup functions or quantities with
;; operators such as "add")
(defun google-c-lineup-expression-plus-4 (langelem)
  "Indents to the beginning of the current C expression plus 4 spaces.

This implements title \"Function Declarations and Definitions\"
of the Google C++ Style Guide for the case where the previous
line ends with an open parenthese.

\"Current C expression\", as per the Google Style Guide and as
clarified by subsequent discussions, means the whole expression
regardless of the number of nested parentheses, but excluding
non-expression material such as \"if(\" and \"for(\" control
structures.

Suitable for inclusion in `c-offsets-alist'."
  (save-excursion
    (back-to-indentation)
    ;; Go to beginning of *previous* line:
    (c-backward-syntactic-ws)
    (back-to-indentation)
    (cond
     ;; We are making a reasonable assumption that if there is a control
     ;; structure to indent past, it has to be at the beginning of the line.
     ((looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
      (goto-char (match-end 1)))
     ;; For constructor initializer lists, the reference point for line-up is
     ;; the token after the initial colon.
     ((looking-at ":\\s *")
      (goto-char (match-end 0))))
    (vector (+ 4 (current-column)))))

(defconst google-c-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . t)
    (comment-column . 40)
    (c-indent-comment-alist . ((other . (space . 2))))
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((arglist-intro google-c-lineup-expression-plus-4)
                        (func-decl-cont . ++)
                        (member-init-intro . ++)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . +)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . /)
                        (innamespace . 0))))
  "Google C/C++ Programming Style.")

(c-add-style "Google" google-c-style)

(defun google-set-c-style ()
  "Set the curernt buffer's c-style to C/C++ Programming
Style. Meant to be added to `c-mode-hook'."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-set-style "Google"))

(c-add-style "windows"
             '("ellemtel"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)))

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq c-basic-offset 8)
  (setq indent-tabs-mode t))

(defun hhp-c-mode ()
  "C mode with adjusted defaults for use with HHP."
  (interactive)
  (c-mode)
  (c-set-style "stroustrup")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

(defvar jkl/cc-style)
(setq jkl/cc-style
      '("stroustrup"
    (indent-tabs-mode . nil)
    (c-offsets-alist . ((innamespace . 0)))))

(c-add-style "vhs-cc-style" jkl/cc-style)

;;; GTAGS
(jkl/cs 'gtags-suggested-key-mapping t)
(let ((en-gtags #'(lambda ()
                    (gtags-mode 1))))
  (add-hook 'c++-mode-hook en-gtags)
  (add-hook 'c-mode-hook en-gtags))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (interactive)
             (hs-minor-mode)))
