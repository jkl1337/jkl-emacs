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
 '(default ((t (:inherit nil :stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :family "Lucida Sans Typewriter")))))

(defun my-add-path (path)
  (setenv "PATH" (concat (getenv "PATH") 
			 (if (string= system-type "windows-nt") ";" ":")
			 (replace-regexp-in-string "/" "\\\\" path)))
  (setq exec-path (nconc exec-path (list path)))
  )

;; additional tool paths for windows
(if (string= system-type "windows-nt")
    (progn
      (setq my-site-lisp-path "c:/prp/emacs-23.2/site-lisp/")
      (mapc 'my-add-path '("C:/prp/Python27"
			   "C:/MingW/msys/1.0/bin"
			   "C:/prp/Mercurial"))
      )
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
)

(setq nxhtml-path (concat my-site-lisp-path "nxhtml/"))

;; Image is everything
(defun my-set-faces  (fl)
  (dolist (faceelt fl)
    (let ((attr-funcs '(set-face-foreground set-face-background))
          (face (car faceelt)))
      (dolist (color (cdr faceelt))
        (funcall (car attr-funcs) face color)
        (setq sfunc (cdr attr-funcs))      
        )
      )
    ))

(setq default-frame-alist
      (append default-frame-alist
              '((width . 130)
                (height . 60)
                (foreground-color . "green")
                (background-color . "black")
                (cursor-color . "white")
                )))

(my-set-faces 
 '((font-lock-type-face "yellow")
   (font-lock-string-face "orange")
   (font-lock-constant-face "plum1")
   (font-lock-variable-name-face "LightGoldenrod")
   (font-lock-function-name-face "turquoise1")
   (font-lock-keyword-face "LightSkyBlue")
   (font-lock-comment-face "Chocolate1")))


;; KEY BINDINGS
(global-set-key "\C-g" 'goto-line)

;; This line binds the function-9 key so that it opens a buffer into
;; gnuplot mode 
(global-set-key [(f9)] 'gnuplot-make-buffer)

(server-start)

;; GENERAL PREFS
(iswitchb-mode 1)
(setq auto-save-interval 3500)
(setq auto-save-timeout nil)

;; MAJOR MODES - add'l major mode setup
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(setq auto-mode-alist
      (cons '("\\.\\([Ff][Rr][Mm]\\|[Bb][Aa][Ss]\\|[Cc][Ll][Ss]\\)$" . 
	      visual-basic-mode) auto-mode-alist))

(setq auto-mode-alist (cons '("\\.gp$" . gnuplot-mode) auto-mode-alist))

;; TODO: restore doc-mode (for doc files with catdoc?) if necessary
;; - verify css-mode
;; - reset css indentation functions

;; ORG-MODE
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; return activates link
(setq org-return-follows-link t)

;; here are for the custom statuses
(setq org-agenda-custom-commands
    '(("w" todo "WAITING" nil)
    ("n" todo "NEXT" nil)
    ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT"))))
)

(defun gtd ()
   (interactive)
   (find-file "~/org/gtd.org")
)

;; LIBRARIES - Heavy weight packages

;; PYTHON - Pymacs and ropemacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(when (string= system-type "windows-nt")
  (require 'yasnippet)
  (yas/initialize)
  (yas/load-directory (concat my-site-lisp-path "yasnippet-0.6.1c/snippets"))
  )

;; NXHTML
(load (concat nxhtml-path "autostart.el"))
(tabkey2-mode)

;; ECB - Code Browser
(require 'ecb)

;; LaTeX
(load "auctex" t t nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'turn-on-reftex)

;; GNUPLOT
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; MACROS
(defun remove-or-convert-trailing-ctl-M ()
  "Propose to remove or convert trailing ^M from a file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\^M" nil t)
        ;; a ^M is found
        (if (or (= (preceding-char) ?\^J)
                (= (following-char) ?\^J))
            ;; Must find a way to display the buffer before this question
            (if (y-or-n-p "Remove trailing ^M ? ")
                (progn (goto-char (point-min))
                       (perform-replace "\^M" "" nil nil nil)
                       (pop-mark))
              (message "No transformation."))
          (if (y-or-n-p "Convert ^M into ^J ? ")
              (progn (goto-char (point-min))
                     (perform-replace "\^M" "\^J" nil nil nil)
                     (pop-mark))
            (message "No transformation.")))
      ;;(message "No ^M in this file !")
      )))

(unless (string= system-name "windows-nt") 
  (add-hook 'find-file-hooks 'remove-or-convert-trailing-ctl-M)
  )

;; PROGRAMMING
;; C styles
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
    )
  "Harris C(++)/IDL style")

(c-add-style "harris" harris-c-style)

(defun harris-c-mode ()
  (interactive)
  (c-set-style "harris")
  (setq tab-width 4
        indent-tabs-mode nil)
  (c-toggle-auto-newline 1))

;;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

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
