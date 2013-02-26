
(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)
(global-set-key [kp-delete] 'delete-char)

(jkl/cs 'mac-control-modifier 'meta
                'mac-option-modifier 'super
		'mac-command-modifier 'control)

(jkl/add-exec-path "/Library/Frameworks/Python.framework/Versions/2.7/bin" t)

(setq user-emacs-directory "/Users/jluebs/emacs/")
(setq jkl/pkg-path (concat user-emacs-directory "pkg/"))
(setq jkl/info-path (concat user-emacs-directory "info/"))

(eval-after-load "slime"
  '(jkl/cs 'inferior-lisp-program "sbcl"))
