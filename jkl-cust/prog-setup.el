
(add-hook 'hs-minor-mode-hook
          '(lambda  ()
             (define-key hs-minor-mode-map (kbd "C-+") 'hs-toggle-hiding)))

;; LUA Mode settings
;; lua-mode install is in top-level
(add-hook 'lua-mode-hook
          (lambda ()
            (interactive)
            (hs-minor-mode)
            (lua-block-mode t)))

;;; Documentation helpers
(when (and (not jkl/mswinp) (featurep 'w3m-load))
  (jkl/set-default
   'browse-url-browser-function
   (list* '("file:.*/usr/local/share/gtk-doc/html" . w3m-browse-url)
          '("file:.*/usr/share/gtk-doc/html" . w3m-browse-url)
          '("file:.*/usr/share/devhelp/books" . w3m-browse-url)
          '("file:.*/usr/share/doc" . w3m-browse-url)
          (if (listp browse-url-browser-function)
              browse-url-browser-function
            `(("." . ,browse-url-browser-function))))))

(autoload 'gtk-lookup-symbol "gtk-look" nil t)
(unless jkl/mswinp
  (jkl/set-default
   'gtk-lookup-devhelp-indices
   '("/usr/share/gtk-doc/html/*/*.devhelp*"
     "/usr/local/share/gtk-doc/html/*/*.devhelp*"
     "/usr/share/devhelp/books/*/*.devhelp*"))
  (define-key global-map [?\C-h ?\C-j] 'gtk-lookup-symbol))

;;;; C styles
(defconst harris-c-style
  '((c-tab-always-indent        . t)
    (c-basic-offset             . 3)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . (
                                   (defun-open before after)
                                   (defun-close before after)
                                   (class-open before after)
                                   (class-close before after)
                                   (inline-open)
                                   (inline-close)
                                   (block-open before after)
                                   (block-close . c-snug-do-while)
                                   (brace-list-open before after)
                                   (brace-list-close before)
                                   (statement-case-open before after)
                                   (substatement-open before after)
                                   (extern-lang-open before after)
                                   (namespace-open before after)
                                   (module-open before after)
                                   (composition-open before after)))

    (c-hanging-colons-alist . ((member-init-intro after)
                               (inher-intro)
                               (case-label after)
                               (label after)
                               (access-label after)))

    (c-cleanup-list . (scope-operator
                       empty-defun-braces
                       defun-close-semi))

    (c-offsets-alist . ((arglist-close . c-lineup-arglist)
                        (arglist-cont . c-lineup-argcont)
                        (arglist-cont-nonempty . (c-lineup-arglist-operators c-lineup-argcont))
                        (substatement-open . 0)
                        (case-label . 0)
                        (block-open . 0)
                        (knr-argdecl-intro . -)))
                     
    (c-echo-syntactic-information-p . t)
    (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-before-nonblanks
                                      c-semi&comma-inside-parenlist))
    )
  "Harris C(++)/IDL style")

(c-add-style "harris" harris-c-style)

(defun harris-c-mode ()
  (interactive)
  (c-set-style "harris")
  (setq tab-width 4
        indent-tabs-mode nil)
  (c-toggle-auto-newline 1))

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

(add-hook 'c-mode-common-hook
          '(lambda ()
             (interactive)
             (hs-minor-mode)))
