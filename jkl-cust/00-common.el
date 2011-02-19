(require 'jkl-util)

(when jkl/env-harris-sc
  (load "host-SD011845.el"))

(when jkl/env-is-mswin
  (face-spec-set 'default '((t (:inherit nil :stipple nil :inverse-video nil
					:box nil :strike-through nil :overline nil :underline nil
					:slant normal :weight normal :height 98 :width normal
					:family "Lucida Sans Typewriter" :foundry "outline" :foreground "green" :background "black"))))
)

(unless jkl/env-is-mswin
  (jkl/set-face 'default '((t (:inherit nil :stipple nil :inverse-video nil
					:box nil :strike-through nil :overline nil :underline nil
					:slant normal :weight normal :height 100 :width normal
					:family "Lucida Sans Typewriter" :foundry "outline" :foreground "green" :background "black"))))
)

(jkl/setv 'default-frame-alist
	  '((width . 130)
	    (height . 60)
	    (foreground-color . "green")
	    (background-color . "black")
	    (cursor-color . "white")
	    ))

(jkl/set-face-colors
 '((font-lock-type-face "yellow")
   (font-lock-string-face "orange")
   (font-lock-constant-face "plum1")
   (font-lock-variable-name-face "LightGoldenrod")
   (font-lock-function-name-face "turquoise1")
   (font-lock-keyword-face "LightSkyBlue")
   (font-lock-comment-face "Chocolate1")))

(iswitchb-mode 1)
(show-paren-mode 1)

(global-set-key (kbd "M-w") 'copy-region-as-kill)

;; GENERAL PREFS
(jkl/setv 'inhibit-startup-screen t
	  'initial-scratch-message nil
	  'auto-save-interval 3000
	  'auto-save-timeout 300
	  'make-backup-files nil)

;;(transient-mark-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(display-time-mode)
(jkl/setv 'display-time-24hr-format t)

;; MAJOR MODES - add'l major mode setup
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(jkl/setv 'auto-mode-alist
	  (nconc '(("\\.\\([Ff][Rr][Mm]\\|[Bb][Aa][Ss]\\|[Cc][Ll][Ss]\\)$" . visual-basic-mode)
		   ("\\.lua$" . lua-mode)
		   ("\\.gp$" . gnuplot-mode))
		 auto-mode-alist))
		   

;; ORG-MODE
(setq load-path (cons (concat jkl/pkg-path "org-mode/lisp")
		      (cons (concat jkl/pkg-path "org-mode/contrib/lisp") load-path)))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-ca" 'org-agenda)

(global-set-key (kbd "<f12>") 'org-agenda)

(define-key global-map [f9] (make-sparse-keymap))

(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> g") 'gnus)

(global-set-key (kbd "C-M-r") 'org-capture)

;; ORG-MODE enable yas/flyspell
(add-hook 'org-mode-hook
	  (lambda ()
	    (set-fill-column 78)
	    (auto-fill-mode 1)
	    (flyspell-mode 1)))

(jkl/setv
 'org-agenda-files '("c:/prp/project/simple.org")
 'org-fast-tag-selection-single-key t
 'org-treat-S-cursor-todo-selection-as-state-change t
 'org-todo-keywords '((sequence "TODO(t!)" "STARTED(s!)" "WAITING(w@/!)" "APPT(a)" "MAYBE(m!)" "|" "DONE(d!/!) CANCELLED(c@/!) DEFFERED(f@/!)")))

(setq org-capture-templates '(("t" "todo" entry (file org-default-notes-file) "* TODO %?\n%U\n%a" :clock-in t :clock-resume t)
			      ("n" "note" entry (file org-default-notes-file) "* %? :NOTE:\n%U\n%a\n:CLOCK:\n:END:" :clock-in t :clock-resume t)
			      ("a" "appointment" entry (file+datetree jkl/org-diary-file) "* %?
%U" :clock-in t :clock-resume t)
			      ("p" "Phone call" entry (file org-default-notes-file) "* Phone %(jkl/phone-call) - %(gjg/bbdb-company) :PHONE:\n%U\n\n%?"
			       :clock-in t :clock-resume t)
			      ("w" "org-protocol" entry (file org-default-notes-file) "* TODO Review %c\n%U" :immediate-finish t :clock-in t :clock-resume t)))


(org-clock-persistence-insinuate)
;(setq org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK"))
;(setq org-clock-into-drawer "CLOCK")
;(setq org-clock-out-remove-zero-time-clocks t)
;(setq org-clock-out-when-done t)
 
(defun gtd ()
   (interactive)
   (find-file "~/org/gtd.org")
)

;; NXHTML
(load (concat (concat jkl/pkg-path "nxhtml/") "autostart.el"))

(when (featurep 'tabkey2)
  (tabkey2-mode t))

;; ECB - Code Browser
(require 'ecb)

;; PYTHON - Pymacs and ropemacs
(when t
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  (pymacs-load "ropemacs" "rope-")
  (jkl/setv 'ropemacs-enable-autoimport t))

;; LaTeX
(load "auctex" t t nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'turn-on-reftex)

;; GNUPLOT
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; HELPFUL

(unless jkl/env-is-mswin
  (add-hook 'find-file-hooks 'jkl/remove-or-convert-trailing-ctl-M)
  )

(server-start)
