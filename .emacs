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

(iswitchb-mode 1)

;;(setq debug-on-error t)
(setq my-site-lisp-path "c:/prp/emacs-23.2/site-lisp/")
(load (concat my-site-lisp-path "nxhtml/autostart.el"))

(require 'ecb)

;; cc-mode for Harris
(defconst harris-c-style
  '((c-tab-always-indent        . t)
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
    
    
    )
  "Harris C(++)/IDL style")

(c-add-style "harris" harris-c-style)


