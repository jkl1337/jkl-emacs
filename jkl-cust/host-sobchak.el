
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)
(global-set-key [kp-delete] 'delete-char)

(jkl/custom-set 'mac-option-modifier 'super
		'mac-command-modifier 'meta)

(setq user-emacs-directory "/Users/jluebs/emacs/")
(setq jkl/pkg-path (concat user-emacs-directory "pkg/"))
(setq jkl/info-path (concat user-emacs-directory "info/"))

(eval-after-load "slime"
  '(jkl/custom-set 'inferior-lisp-program "sbcl"))
