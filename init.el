;; FIXME: prevent duplicate load-path nonsense on Harris SC setup, what with using site-lisp and all
;; TODO: change paren mode highlight faces
;; see face-user-default-spec in faces.el TODO

(defvar emacs-debug-loading t)

(defconst jkl/mswinp
  (eq system-type 'windows-nt))

(defvar jkl/pkg-path (concat user-emacs-directory "pkg/")
  "The install directory for user-local 3rd-party packages")

(defvar jkl/info-path (concat user-emacs-directory "info/")
  "Additional info directory path")

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
      (load (concat jkl/my-dir "jkl-cust/" file) noerror nil t))
    (setq files (cdr files))))

(defun jkl/script-dir ()
  (concat jkl/my-dir "jkl-cust/"))

(add-to-list 'load-path (concat jkl/my-dir "jkl-lib"))
(require 'jkl-util)

;; Misc GIT controlled 3rd party scripts
(let ((contrib-dir (concat jkl/my-dir "contrib/")))
  (jkl/load-path-add-immediate-subdirs contrib-dir)
  (add-to-list 'load-path contrib-dir))

;; load host specific early-init
(jkl/load-script (concat "host-" system-name ".el") t)

;;;; ELPA
;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;      (package-initialize))

;; configure standard 3rd party install load path
(jkl/load-path-add-immediate-subdirs jkl/pkg-path)
(add-to-list 'load-path jkl/pkg-path)

;; Request to merge custom info.
;; Consider setting additional-path with default list in order to
;; have custom docs separated (put them in INFOPATH)
(add-to-list 'Info-default-directory-list 
	     (expand-file-name jkl/info-path))

;;;; Command/environment customizations
;; Windows msys shell
(when jkl/mswinp
  (let ((msys-bash-exe (locate-file "bash.exe" exec-path)))
    (when (string-match-p "/MSYS/" (upcase msys-bash-exe))
      (jkl/set-default 'shell-file-name (file-name-sans-extension msys-bash-exe))
      (jkl/set-default 'explicit-shell-file-name shell-file-name)
      (jkl/set-default 'explicit-bash-args '("--login" "--noediting" "-i"))
      (jkl/set-default
       'shell-mode-hook '(lambda ()
			   (tabkey2-mode nil)
			   (ansi-color-for-comint-mode-on)
			   ;;(setq comint-scroll-show-maximum-output 'this)
			   (make-variable-buffer-local 'comint-completion-addsuffix)
			   (setq comint-completion-addsuffix t)
			   (setq w32-quote-process-args ?\"))))))

; For non-Windows at least make sure we have ANSI escapes working
(unless jkl/mswinp
  (jkl/set-default 'shell-mode-hook 'ansi-color-for-comint-mode-on))

; Going to try out Ctrl-Tab for a bit as a replacement for M-tab

(defvar jkl/mswin-C-tab-or-disable-alt-win t
  "If true, use C-tab as a replacement for M-tab, otherwise, let's
try disabling Alt-Tab switching and see how that works")

(when jkl/mswinp
  (if jkl/mswin-C-tab-or-disable-alt-win
      (define-key function-key-map [(control tab)] [?\M-\t])
    (w32-register-hot-key [M-tab])))

;;;; w3m
(require 'w3m-load nil t)
(when (and (not jkl/mswinp) (featurep 'w3m-load))
  (jkl/set-default
   'browse-url-browser-function
   (nconc (list 
	   '("http://.*docs.python.org/". w3m-browse-url)
	   '("file:.*/usr/local/share/gtk-doc/html" . w3m-browse-url)
	   '("file:.*/usr/share/gtk-doc/html" . w3m-browse-url)
	   '("file:.*/usr/share/devhelp/books" . w3m-browse-url))
	  (if (listp browse-url-browser-function)
	      browse-url-browser-function
	    `(("." . ,browse-url-browser-function))))))

;;;; BBDB
(require 'bbdb)

;;;; ORG-MODE
(when (car (jkl/try-add-pkg "org-mode/lisp" "org-mode/contrib/lisp"))
  (let ((org-info-dir (concat jkl/pkg-path "org-mode/doc")))
    (when (file-readable-p (concat org-info-dir "/dir"))
      (add-to-list 'Info-default-directory-list
	    (expand-file-name org-info-dir)))))

(require 'org-install nil)

(jkl/set-default default-major-mode 'org-mode)
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
			       :slant normal :weight normal :height 90 :width normal
			       :foundry "*" :family "Lucida Sans Typewriter")))))

(unless jkl/mswinp
  (let ((font-param))
    ;;(setq font-param '("ProggyCleanTT" . 120))
    (setq font-param '("Lucida Sans Typewriter" . 90))
    (jkl/set-face 'default `((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil
				 :overline nil :underline nil :background "black" :foreground "green" 
				 :slant normal :weight normal :height ,(cdr font-param) :width normal
				 :foundry "*" :family ,(car font-param)))))))

(jkl/set-default 'default-frame-alist
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

;; END appearance / basic faces

;;;; GLOBAL settings
(jkl/set-default 'fill-column 72)
;; going to go ahead and default to no tabs globally
(jkl/set-default 'indent-tabs-mode nil)

;;;; YASNIPPET
(require 'yasnippet)
(yas/initialize)
(let ((snippet-dir (concat jkl/pkg-path "yasnippet-0.6.1c/snippets")))
  (when (file-accessible-directory-p snippet-dir)
    (yas/load-directory snippet-dir)))

;;;; NXHTML
(when (load (concat jkl/pkg-path "nxhtml/" "autostart.el") t t t)
  (tabkey2-mode t))

;;;; CEDET and ECB
;;; my site CEDET
(load (concat jkl/pkg-path "cedet-1.0/common/cedet") nil)

;;; ECB - Code Browser
(require 'ecb-autoloads nil t)

;;;; CUSTOM MAJOR MODES
;;; MAJOR MODES - add'l major mode setup
(autoload 'lua-block-mode "lua-block" "Lua highlight matching block")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(jkl/add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;;; BEGIN CUSTOMIZATION
(jkl/add-to-list 'auto-mode-alist
		 '("\\.\\([Ff][Rr][Mm]\\|[Bb][Aa][Ss]\\|[Cc][Ll][Ss]\\)$" . visual-basic-mode))
(jkl/add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(jkl/add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))

(global-set-key (kbd "M-w") 'copy-region-as-kill)

(iswitchb-mode 1)
(show-paren-mode 1)
;;(transient-mark-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(display-time-mode)
(jkl/set-default 'display-time-24hr-format t)

(jkl/set-default 'inhibit-startup-screen t
		 'initial-scratch-message nil
		 'auto-save-interval 3000
		 'auto-save-timeout 300
		 'make-backup-files nil)

;; ELISP customization
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (interactive)
	     (turn-on-eldoc-mode)
	     (define-key emacs-lisp-mode-map [f5] 'eval-region)
	     (define-key emacs-lisp-mode-map (kbd "C-c <f5>") 'eval-buffer)))

;; git support / non-Windows only for now
(unless jkl/mswinp
  (require 'git)
  (autoload 'git-blame-mode "git-blame"
    "Minor mode for incremental blame for Git." t))

;;; A find-file-hook that is interactive is arguably completely evil
;;; but perhaps I will assign this to a keybinding, later
;; (unless jkl/mswinp
;;   (add-hook 'find-file-hook 'jkl/remove-or-convert-trailing-ctl-M))

(jkl/load-scripts 
 "org-setup.el"
 ;;"cedet-setup.el"
 "doc-setup.el"
 "py-setup.el"
 "prog-setup.el"
 )

(setq custom-file (concat jkl/my-dir "custom.el"))
(load custom-file)

(server-start)
