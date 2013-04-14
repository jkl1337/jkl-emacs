;; TODO: change paren mode highlight faces
;; see face-user-default-spec in faces.el TODO
;; TODO: Change the require predicates to use fboundp and support lazy loading
;; FIXME: have a separate savefile directory

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
  (load (concat jkl/my-dir "jkl-cust/" script) noerror nil nil))

(defun jkl/load-scripts (&rest files)
  (while files
    (let ((file (car files))
          (noerror))
      (when (listp file)
        (setq noerror (cadr file))
        (setq file (car file)))
      (load (concat jkl/my-dir "jkl-cust/" file) noerror nil nil))
    (setq files (cdr files))))

(defun jkl/script-dir ()
  (concat jkl/my-dir "jkl-cust/"))

(add-to-list 'load-path (concat jkl/my-dir "jkl-lib"))
(require 'jkl-util)

;;;; Misc 3rd party scripts
(let ((contrib-dir (concat jkl/my-dir "contrib/")))
  (jkl/load-path-add-immediate-subdirs contrib-dir)
  (add-to-list 'load-path contrib-dir))

;; Snippet, testing keymap
(setq jkl/func-map (make-sparse-keymap))
(define-key (current-global-map) [f6] jkl/func-map)

;; load host specific early-init
(jkl/load-script (concat "host-"
			 (substring system-name 0 (string-match "\\." system-name))
			 ".el") t)

;; BEGIN APPEARANCE / BASIC FACES - with fallback when color-theme is not installed
(jkl/cs 'use-file-dialog nil)

(let ((font (cond
             (jkl/mswinp "Lucida Console-9")
             ((eq 'darwin system-type) "Monaco-9")
             (t "Monaco-8"))))
  (when font (set-face-attribute 'default nil :font font)))

;; (jkl/cs 'default-frame-alist
;;                 '((width . 140)
;;                   (height . 50)
;;                   (foreground-color . "green")
;;                   (background-color . "black")
;;                   (cursor-color . "white")))

(jkl/cs 'custom-theme-directory (concat jkl/my-dir "themes/"))
(condition-case nil
    (load-theme 'cool-dark t nil)
  (error nil))

(defun jkl/font-switch ()
  (interactive)
  (let* ((cur-height (face-attribute 'default :height))
         (new-height
          (if (< cur-height 96) 96 72)))
    (set-face-attribute 'default nil :height new-height)))
(define-key (current-global-map) (kbd "C-+") 'text-scale-increase)
(define-key (current-global-map) (kbd "C--") 'text-scale-decrease)
(define-key jkl/func-map "f" 'jkl/font-switch)

;; END appearance / basic faces

;;;; EL-GET
(jkl/cs 'el-get-dir (expand-file-name (concat user-emacs-directory "el-get/")))
(jkl/cs 'el-get-git-shallow-clone t)

;;; bootstreap CEDET early
(let ((cedet-load-file (concat (expand-file-name "cedet" el-get-dir) "/cedet-devel-load.el")))
  (when (file-exists-p cedet-load-file)
    (load-file cedet-load-file)))

;;; Bootstrap el-get
(add-to-list 'load-path (concat el-get-dir "el-get"))
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;;; el-get recipes
(setq el-get-sources
      '(
	(:name auto-complete
	       :post-init (progn
			    (require 'auto-complete)
			    (add-to-list 'ac-dictionary-directories
					 (expand-file-name "dict"))
			    (require 'auto-complete-config)
			    (ac-config-default)))
	(:name cedet
	       :build/darwin `("touch `find . -name Makefile`"
			       ,(concat "make EMACS=" el-get-emacs))
               :load-path nil)
        (:name pymacs
               :build `(,(concat "make"
                                 (if (file-executable-p "/usr/bin/python2")
                                     " PYTHON=python2" " "))))
        (:name ecb
               :depends cedet
               :build `(("make" "CEDET=" ,(concat el-get-dir "cedet/lisp/common/")
                         ,(concat "EMACS=" (shell-quote-argument el-get-emacs)))))
        (:name python-mode
               ;; :url "https://github.com/jkl1337/python-mode.git"
               :depends highlight-indentation
               :compile ("python-mode.el" "python-extended-executes.el" "test/doctest-mode.el"
                         "completion" "extensions")
               )

        (:name lua-mode
               :branch "jkl_indent"
               :url "https://github.com/jkl1337/lua-mode")
        (:name markdown-mode
               :url "https://github.com/milkypostman/markdown-mode")

	(:name csharp-mode
	       :website "https://code.google.com/p/csharpmode/"
	       :description "This is a mode for editing C# in emacs. It's based on cc-mode, v5.30.3 and above."
	       :type git
               :url "luebsj@luebsphoto.com:/srv/git/csharp-mode.git"
	       :features csharp-mode)
	(:name html5
               :after (eval-after-load
                          "rng-loc"
                        '(add-to-list 'rng-schema-locating-files (concat el-get-dir "html5/schemas.xml"))))
        (:name "multi-web-mode"
               :pkgname "fgallina/multi-web-mode")
        (:name flymake-easy
               :type github
               :description "Helpers for easily building Emacs flymake checkers"
               :pkgname "purcell/flymake-easy"
               :website "http://github.com/purcell/flymake-easy")
        (:name rsense
               :build ("ant")
               :compile "etc"
               :post-init (progn
                            (setq rsense-home (concat el-get-dir "rsense"))))

        (:name test-simple
               :description "Unit Test Framework for Emacs Lisp"
               :type github
               :pkgname "rocky/emacs-test-simple"
               :build ("./autogen.sh" "./configure" "make"))

        (:name dbgr
               :depends (test-simple load-relative loc-changes)
               :features (loc-changes load-relative test-simple))
        (:name pkgbuild-mode
               :lazy t)
        ;; (:name org-mode
        ;;        :url "https://github.com/jkl1337/org-mode"
        ;;        :shallow nil
        ;;        :checkout "flyspell")
        (:name yasnippet
               :url "https://github.com/jkl1337/yasnippet"
               :checkout "textmate")
      ))

(progn
  (setq jkl/el-get-packages
        '(el-get fuzzy popup cedet escreen auto-complete
                 smex
                 helm projectile undo-tree
                 pkgbuild-mode
                 flymake ; wow, the emacs one is a POS
                 flymake-coffee flymake-haml flymake-shell
                 ido-ubiquitous
                 bbdb org-mode ;; bbdb-vcard
                 rainbow-mode rainbow-delimiters
                 markdown-mode nxhtml org-mode pylookup python-mode pymacs lua-mode
                 flymake-easy
                 rcodetools rvm rdebug rinari rhtml-mode rspec-mode yari rsense ruby-block
                 robe-mode
                 yaml-mode haml-mode
                 emms xcscope slime yasnippet csharp-mode jquery-doc
                 html5 js2-mode multi-web-mode coffee-mode
                 sass-mode scss-mode
                 magit
                 clojure-mode nrepl ac-nrepl paredit))

  (el-get 'sync jkl/el-get-packages))

(byte-recompile-directory (jkl/script-dir) 0)
(condition-case nil
    (byte-recompile-directory (concat jkl/my-dir "contrib/") 0)
  (error (warn "Failed to byte-compile some contrib files")))

;;;; CUSTOM KEY BINDINGS
(defalias 'qrr 'query-replace-regexp)
(define-key jkl/func-map "c" 'recompile)

;; Flymake error nav
(defun jkl/flymake-show-next ()
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-err-menu-for-current-line))

(defun jkl/flymake-show-prev ()
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-err-menu-for-current-line))

(define-key jkl/func-map "5" 'jkl/flymake-show-next)
(define-key jkl/func-map "6" 'jkl/flymake-show-prev)

;;;; Command/environment customizations
;; Windows msys shell
(when jkl/mswinp
  (let ((msys-bash-exe (locate-file "bash.exe" exec-path)))
    (when (string-match-p "/MSYS/" (upcase msys-bash-exe))
      (jkl/cs 'shell-file-name (file-name-sans-extension msys-bash-exe))
      (jkl/cs 'explicit-shell-file-name shell-file-name)
      (jkl/cs 'explicit-bash-args '("--login" "--noediting" "-i"))
      (jkl/cs
       'shell-mode-hook '(lambda ()
                           (ansi-color-for-comint-mode-on)
                           ;;(setq comint-scroll-show-maximum-output 'this)
                           (make-variable-buffer-local 'comint-completion-addsuffix)
                           (setq comint-completion-addsuffix t)
                           (setq w32-quote-process-args ?\"))))))

;; For non-Windows at least make sure we have ANSI escapes working
(unless jkl/mswinp
  (jkl/cs 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;; Going to try out Ctrl-Tab for a bit as a replacement for M-tab

(defvar jkl/mswin-C-tab-or-disable-alt-win t
  "If true, use C-tab as a replacement for M-tab, otherwise, let's
try disabling Alt-Tab switching and see how that works")

(when jkl/mswinp
  (if jkl/mswin-C-tab-or-disable-alt-win
      (define-key function-key-map [(control tab)] [?\M-\t])
    (w32-register-hot-key [M-tab])))

;;;; IELM
(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;;;; MARKDOWN-MODE
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;;; browser.el
;; LXDE xdg-open goes through PCManFM which chokes on file URLs containng
;; fragments. Kinda sucks
(defadvice browse-url-can-use-xdg-open (around activate)
  (if (or (member (getenv "DESKTOP_SESSION") '("LXDE" "Lubuntu"))
          (equal (getenv "XDG_CURRENT_DESKTOP") "LXDE"))
      nil
    ad-do-it))

;;;; nxml
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files (concat jkl/my-dir "schemas/schemas.xml")))

;;;; EMMS
;;; FIXME: Reconfigure EMMS

;;;; GIT-BLAME
(jkl/cs 'git--timer-sec 5.0)

;;;; BBDB

;;;; ORG-MODE
;;; FIXME: See about lazy

(jkl/cs 'major-mode 'text-mode)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-ca" 'org-agenda)

(global-set-key (kbd "<f12>") 'org-agenda)

(define-key (current-global-map) [f9] (make-sparse-keymap))

(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> g") 'gnus)

(global-set-key (kbd "C-M-r") 'org-capture)

;;;; GTAGS
(autoload 'gtags-mode "gtags" "" t)

;;;; YASNIPPET
(let ((snippets-dir (concat jkl/my-dir "snippets/")))
  (jkl/cs 'yas-snippet-dirs (list (concat snippets-dir "base") (concat snippets-dir "ext"))
          'yas-prompt-functions '(yas-dropdown-prompt
                                  yas-ido-prompt yas-completing-prompt yas-no-prompt)))

(yas-global-mode)

;;;; PROJECTILE (ELPA)
;; TODO: save file location
(projectile-global-mode)

(require 'helm-projectile)

(defun jkl/helm ()
  "Configure helm"
  (interactive)
  (condition-case nil
      (if (projectile-project-root)
          (helm-other-buffer '(helm-c-source-projectile-files-list
                               helm-c-source-projectile-buffers-list
                               helm-c-source-buffers-list
                               helm-c-source-recentf
                               helm-c-source-buffer-not-found)
                             "*helm prelude*")
        ;; fallback to helm-mini
        (helm-mini))
    (error (helm-mini))))

;;;; NXHTML
(jkl/cs 'mumamo-chunk-coloring 10)

(when (and (= emacs-major-version 24)
           (>= emacs-minor-version 2))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

;;;; AUTO-COMPLETE

(eval-after-load "auto-complete"
  '(progn
     ;; HACK! This is for ropemacs when the project is not already created
     ;; without this the keymap will not allow one to do anything
     (defadvice read-directory-name (before read-directory-cancel-ac-timer activate)
       (ac-cancel-timer))

     (define-key ac-completing-map "\C-n" 'ac-next)
     (define-key ac-completing-map "\C-p" 'ac-previous)
     (define-key ac-completing-map "\M-n" nil)
     (define-key ac-completing-map "\M-p" nil)
     (define-key ac-completing-map "\t" 'ac-funky-expand)
     (define-key ac-completing-map [tab] 'ac-funky-expand)))

(jkl/cs 'helm-c-yas-display-key-on-candidate t
        'helm-c-yas-space-match-any-greedy t)

(autoload 'helm-c-yas-complete "helm-c-yasnippet"
    "List of yasnippet snippets using `helm' interface."
    (interactive))
(global-set-key (kbd "C-c y") 'helm-c-yas-complete)

(jkl/cs 'ac-quick-help-delay 1.0)

;;(ac-set-trigger-key "\s-\t")
(defun ac-js2-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
(add-hook 'js2-mode-hook 'ac-js2-mode-setup)

(defun jkl/ac-objc-mode-setup ()
  (when (require 'auto-complete-clang nil t)
    (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources))))
;;(add-hook 'objc-mode-hook 'jkl/ac-objc-mode-setup)

;; javascript mode - inhibit nxhtml
(setq auto-mode-alist
      (remove '("\\.js\\'" . javascript-mode) auto-mode-alist))

;;;; JDEE

;;;; CUSTOM MAJOR MODES

;;; MAJOR MODES - add'l major mode setup

;; LUA Mode
(autoload 'lua-block-mode "lua-block" "Lua highlight matching block")
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(let ((jlua-sh "~/bin/jlua"))
  (when (file-executable-p jlua-sh)
    (jkl/cs 'lua-default-application (expand-file-name jlua-sh)))
  (when (executable-find "luajit")
    (setenv "LUAI" "luajit")))

;;;; BEGIN CUSTOMIZATION

(defalias 'yes-or-no-p 'y-or-n-p)

(when (bound-and-true-p jkl/use-hunspell)
  (eval-after-load "pspell"
    (progn
      (let ((hunspell-program (executable-find "hunspell")))
        (when hunspell-program
          (jkl/cs 'ispell-extra-args '("-a" "-i" "utf-8")
                          'ispell-silently-savep t
                          'ispell-program-name hunspell-program))))))
(blink-cursor-mode 0)

;;; TRAMP administer root files on remote hosts
(jkl/add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:luebsj@%h:"))
(jkl/add-to-list 'tramp-default-proxies-alist '((regexp-quote (system-name)) nil nil))

(eval-after-load "tramp"
  '(progn
     (defvar sudo-tramp-prefix
       "/sudo:"
       (concat "Prefix to be used by sudo commands when building tramp path "))

     (defun sudo-file-name (filename)
       (let* ((splitname (split-string filename ":"))
              (components (or (cdr splitname)
                              (cons (concat "root@" (system-name)) splitname))))
         (concat sudo-tramp-prefix (mapconcat 'identity components ":"))))

     (defun sudo-find-file (filename &optional wildcards)
       "Calls find-file with filename with sudo-tramp-prefix prepended"
       (interactive "fFind file with sudo ")
       (let ((sudo-name (sudo-file-name filename)))
         (apply 'find-file
                (cons sudo-name (if (boundp 'wildcards) '(wildcards))))))

     (defun sudo-reopen-file ()
       "Reopen file as root by prefixing its name with sudo-tramp-prefix and by clearing buffer-read-only"
       (interactive)
       (let*
           ((file-name (expand-file-name buffer-file-name))
            (sudo-name (sudo-file-name file-name)))
         (progn
           (setq buffer-file-name sudo-name)
           (rename-buffer sudo-name)
           (setq buffer-read-only nil)
           (message (concat "File name set to " sudo-name)))))

     (global-set-key "\C-x+" 'sudo-find-file)
     (global-set-key "\C-x!" 'sudo-reopen-file)))

;;; GLOBAL settings
(find-function-setup-keys)
(define-key 'help-command "\C-f" 'find-function)
(define-key 'help-command "\C-v" 'find-variable)

;; SMEX!

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-x ^") 'join-line)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-x M-m") 'shell)

(global-set-key "\C-xg" 'magit-status)
(global-set-key "\C-x\C-b" 'ibuffer)

(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(global-set-key "\C-c\C-d" (lambda ()
                              (interactive)
                              (let ((cur-window (selected-window)))
                                (other-window 1)
                                (quit-window)
                                (select-window cur-window))))

(global-set-key "\M-/" 'hippie-expand)

(jkl/cs 'hippie-expand-try-functions-list '(try-expand-dabbrev
                                            try-expand-dabbrev-all-buffers
                                            try-expand-dabbrev-from-kill
                                            try-complete-file-name-partially
                                            try-complete-file-name
                                            try-expand-all-abbrevs
                                            try-expand-list
                                            try-expand-line
                                            try-complete-lisp-symbol-partially
                                            try-complete-lisp-symbol))

(ido-mode 1)
(ido-ubiquitous-mode 1)
(ido-ubiquitous-disable-in execute-extended-command)
(jkl/cs 'ido-enable-prefix nil
        'ido-enable-flex-matching t
        'ido-create-new-buffer 'always
        'ido-use-filename-at-point 'guess
        'ido-max-prospects 10
        'ido-default-file-method 'selected-window)

(electric-pair-mode 1)

;; unique buffer names
(require 'uniquify)
(jkl/cs 'uniquify-buffer-name-style 'forward
        'uniquify-separator "/"
        'uniquify-after-kill-buffer-p t
        'uniquify-ignore-buffers-re "^\\*")

;; savehist
(jkl/cs 'savehist-additional-variables '(search ring regexp-search-ring)
        'savehist-autosave-interval 60)
;;        'savehist-file
(savehist-mode 1)

(jkl/cs 'recentf-max-saved-items 200
        'recentf-max-menu-items 15)
(recentf-mode 1)

(jkl/cs 'fill-column 72)
;; going to go ahead and default to no tabs globally
(jkl/cs 'indent-tabs-mode nil)

;;; show-trailing-whitespace
(jkl/cs 'show-trailing-whitespace t)

(show-paren-mode 1)
(global-hl-line-mode 1)

(mapc #'(lambda (s) (put s 'disabled nil))
      '(narrow-to-region narrow-to-page narrow-to-defun
                         upcase-region downcase-region))

;; cleanup old buffers
(require 'midnight)

(global-undo-tree-mode)

(winner-mode 1)

(display-time-mode)

(jkl/cs 'display-time-24hr-format t)

(jkl/cs 'backup-directory-alist
        `((".*" . ,temporary-file-directory))
	'auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

(jkl/cs 'inhibit-startup-screen t
        'initial-scratch-message nil
        'auto-save-interval 3000
        'auto-save-timeout 300)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;;; UI
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(jkl/cs 'scroll-margin 0
        'scroll-conservatively 100000
        'scroll-preserve-screen-position 1)

(jkl/cs 'frame-title-format
        '(" Emacs - " (:eval (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b"))))

(add-to-list 'interpreter-mode-alist '("dash" . sh-mode))

(add-to-list 'auto-mode-alist
                 '("\\.\\([Ff][Rr][Mm]\\|[Bb][Aa][Ss]\\|[Cc][Ll][Ss]\\)$" . visual-basic-mode))
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.rockspec$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))

;; arch and pacman PKGBUILD files
(add-to-list 'auto-mode-alist (cons "/PKGBUILD$"
                                    (if (fboundp 'pkgbuild-mode)
                                        'pkgbuild-mode
                                      #'(lambda ()
                                          (sh-mode)
                                          (sh-set-shell "bash" t)))))

(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))

;;;; DIRED
(put 'dired-find-alternate-file 'disabled nil)

;;;; EDIFF
(jkl/cs 'ediff-window-setup-function 'ediff-setup-windows-plain)

;;; ELISP customization

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (interactive)
             (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
             (define-key emacs-lisp-mode-map (kbd "C-c <f5>") 'eval-buffer)))

(add-hook 'emacs-lisp-mode-hook 'jkl/recompile-elc-on-save)

;;;; PERFORCE
;; Adding this backend causes indefinite wait in all non-rev controlled directories
(jkl/cs 'vc-p4-require-p4config t)
;; Damn, it's not working well
;; (jkl/add-to-list 'vc-handled-backends 'P4) 

(jkl/load-scripts 
 "org-setup"
 "cedet-setup"
 "ruby-setup"
 "doc-setup"
 "py-setup"
 "prog-setup"
 "clisp-setup"
 "clojure-setup"
 "web-setup"
 )

(setq custom-file (concat jkl/my-dir "custom.el"))
(unless (file-exists-p custom-file)
  (with-temp-file custom-file))
(load custom-file)

(server-start)
