
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
(when (featurep 'w3m-load)
  (jkl/custom-set
   'browse-url-browser-function
   (nconc
    (unless jkl/mswinp
      ;; Non-windows platform only
      (list
       '("file:.*/usr/local/share/gtk-doc/html" . w3m-browse-url)
       '("file:.*/usr/share/gtk-doc/html" . w3m-browse-url)
       '("file:.*/usr/share/devhelp/books" . w3m-browse-url)
       '("file:.*/usr/share/doc" . w3m-browse-url)
       '("\\(file://\\)?/.*/hyperspec" . w3m-browse-url)))
    ;; Common set
    (list
     '("http://www\\.clisp\\.org/impnotes" . w3m-browse-url)
     '("http://clisp\\.cons\\.org/impnotes" . w3m-browse-url)
     '("http://www\\.lispworks\\.com/reference/HyperSpec" . w3m-browse-url)
     `("." . ,(if jkl/mswinp 'browse-url-default-windows-browser
                'browse-url-default-browser))))))


(autoload 'gtk-lookup-symbol "gtk-look" nil t)
(unless jkl/mswinp
  (jkl/custom-set
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
(c-add-style "windows"
             '("ellemtel"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)))

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

(defvar jkl/cc-style)
(setq jkl/cc-style
      '("stroustrup"
    (indent-tabs-mode . nil)
    (c-offsets-alist . ((innamespace . 0)))))

(c-add-style "vhs-cc-style" jkl/cc-style)

;;; GTAGS
(jkl/custom-set 'gtags-suggested-key-mapping t)
(let ((en-gtags #'(lambda ()
                    (gtags-mode 1))))
  (add-hook 'c++-mode-hook en-gtags)
  (add-hook 'c-mode-hook en-gtags))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (interactive)
             (hs-minor-mode)))
