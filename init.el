;; FIXME: prevent duplicate load-path nonsense on Harris SC setup, what with using site-lisp and all
;; TODO: change paren mode highlight faces
;; see face-user-default-spec in faces.el TODO

(defvar emacs-debug-loading t)

(defconst jkl/mswinp
  (eq system-type 'windows-nt))

(defvar jkl/pkg-path (concat user-emacs-directory "pkg/")
  "The install directory for user-local 3rd-party packages")

(defvar jkl/my-dir
  (file-name-directory (file-truename load-file-name)))

(defvar jkl/git-repo-dir
  (file-name-directory (directory-file-name jkl/my-dir)))

(defun jkl/load-script (script &optional noerror)
  (load (concat jkl/my-dir "jkl-cust/" script) noerror nil t))

(defun jkl/load-scripts (&rest files)
  (while files
    (let ((file (car files))
	  (noerror))
      (when (listp file)
	(setq noerror (cadr file))
	(setq file (car file)))
      (load (concat jkl/my-dir "jkl-cust/" file) noerror nil t)
    (setq files (cdr files)))))

(defun jkl/script-dir ()
  (concat jkl/my-dir "jkl-cust/"))

(add-to-list 'load-path (concat jkl/my-dir "jkl-lib"))
(require 'jkl-util)

;; load host specific early-init
(jkl/load-script (concat "host-" system-name ".el") t)

;; configure standard 3rd party install load path
(jkl/load-path-add-immediate-subdirs jkl/pkg-path)
(add-to-list 'load-path jkl/pkg-path)

;; ORG-MODE
(mapc (lambda (dir)
	(let ((dir (concat jkl/pkg-path dir)))
	  (when (file-accessible-directory-p dir)
	    (add-to-list 'load-path dir))))
      '("org-mode/lisp" "org-mode/contrib/lisp"))
(require 'org-install nil)
(jkl/setv default-major-mode 'org-mode)
(jkl/add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

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

;; BEGIN APPEARANCE / BASIC FACES
(when jkl/mswinp
  (jkl/set-face 'default '((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil
			       :overline nil :underline nil :background "black" :foreground "green" 
			       :slant normal :weight normal :height 98 :width normal
			       :foundry "*" :family "Lucida Sans Typewriter")))))

(unless jkl/mswinp
  (jkl/set-face 'default '((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil
			       :overline nil :underline nil :background "black" :foreground "green" 
			       :slant normal :weight normal :height 98 :width normal
			       :foundry "*" :family "Lucida Sans Typewriter")))))

(jkl/setv 'default-frame-alist
	  '((width . 140)
	    (height . 50)
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

;; END APPEARANCE / BASIC FACES

;; YASNIPPET
(require 'yasnippet)
(yas/initialize)
(let ((snippet-dir (concat jkl/pkg-path "yasnippet-0.6.1c/snippets")))
  (when (file-accessible-directory-p snippet-dir)
    (yas/load-directory snippet-dir)))

;; NXHTML
(when (load (concat jkl/pkg-path "nxhtml/" "autostart.el") t t t)
  (tabkey2-mode t))

;; ECB - Code Browser
(require 'ecb-autoloads nil t)

;; CUSTOM MAJOR MODES
;; MAJOR MODES - add'l major mode setup
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(jkl/add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; BEGIN CUSTOMIZATION
(jkl/setv 'auto-mode-alist
	  (nconc '(("\\.\\([Ff][Rr][Mm]\\|[Bb][Aa][Ss]\\|[Cc][Ll][Ss]\\)$" . visual-basic-mode)
		   ("\\.lua$" . lua-mode)
		   ("\\.gp$" . gnuplot-mode)
		   ) auto-mode-alist))

(global-set-key (kbd "M-w") 'copy-region-as-kill)

(iswitchb-mode 1)
(show-paren-mode 1)
;;(transient-mark-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(display-time-mode)
(jkl/setv 'display-time-24hr-format t)

(jkl/setv 'inhibit-startup-screen t
	  'initial-scratch-message nil
	  'auto-save-interval 3000
	  'auto-save-timeout 300
	  'make-backup-files nil)

(unless jkl/mswinp
  (add-hook 'find-file-hooks 'jkl/remove-or-convert-trailing-ctl-M))

(jkl/load-scripts 
 "org-setup.el"
 "doc-setup.el"
 "py-setup.el"
 "prog-setup.el"
 )

(setq custom-file (concat jkl/my-dir "custom.el"))
(load custom-file)

(server-start)
