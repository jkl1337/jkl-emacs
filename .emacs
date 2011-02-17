(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "outline" :family "Inconsolata")))))

(server-start)

(setq default-frame-alist
      (append default-frame-alist
	      '((foreground-color . "green")
		(background-color . "black")
		)))

;; FIXME
(set-face-foreground 'cursor "white")
(set-face-background 'cursor "blue")

;; nicer buffer switching
(iswitchb-mode 1)

;; additional tool paths for windows
(setq my-site-lisp-path "c:/prp/emacs-23.2/site-lisp/")
 
(defun jkl-add-path (path)
  (setenv "PATH" (concat (getenv "PATH") ":"
			 (replace-regexp-in-string "/" "\\\\" path)))
  (setq exec-path (nconc exec-path (list path)))
  )

(mapcar 'jkl-add-path '("C:/prp/Python27" "C:/MingW/msys/1.0/bin"))

;; NXHTML
(load (concat my-site-lisp-path "nxhtml/autostart.el"))

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat my-site-lisp-path "yasnippet-0.6.1c/snippets"))

;; Pymacs and ropemacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(pymacs-load "ropemacs" "rope-")


(require 'ecb)

;; cc-mode for Harris
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
			(
			(substatement-open . 0)
			(case-label . 0)
			(block-open . 0)
			(knr-argdecl-intro . -)))
		     
    (c-echo-syntactic-information-p . t)
    

    
    )
  "Harris C(++)/IDL style")

(c-add-style "harris" harris-c-style)


(defun my-c-mode-common-hook ()
  (c-set-style "harris")
  (setq tab-width 4
	indent-tabs-mode nil)
  (c-toggle-auto-newline 1))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)