;; TODO: change paren mode highlight faces
;; see face-user-default-spec in faces.el TODO
;; TODO: Change the require predicates to use fboundp and support lazy loading

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
(let ((font (cond
             (jkl/mswinp "Lucida Console-9")
             ((eq 'darwin system-type) "Monaco-9")
             (t "Monaco-8"))))
  (when font (set-face-attribute 'default nil :font font)))

(jkl/custom-set 'default-frame-alist
                '((width . 140)
                  (height . 50)
                  (foreground-color . "green")
                  (background-color . "black")
                  (cursor-color . "white")))

(jkl/set-face-colors
 '((font-lock-type-face "yellow")
   (font-lock-string-face "orange")
   (font-lock-constant-face "plum1")
   (font-lock-variable-name-face "LightGoldenrod")
   (font-lock-function-name-face "turquoise1")
   (font-lock-keyword-face "LightSkyBlue")
   (font-lock-comment-face "Chocolate1")))

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
(jkl/custom-set 'el-get-dir (concat user-emacs-directory "el-get/"))
(jkl/custom-set 'el-get-git-shallow-clone t)

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
	(:name jdee
               :website "http://jdee.sourceforge.net/"
               :description "The JDEE is an add-on software package that turns Emacs into a comprehensive system for creating, editing, debugging, and documenting Java applications."
               :type git
               :depends cedet
               :url "http://github.com/jkl1337/jdee.git"
               :build `(,(concat
			  "CLASSPATH="
			  (let ((cp "/usr/share/java/apache-ant/lib/ant-contrib-1.0b3.jar"))
			    (when (file-exists-p cp) cp))
			  " ant bindist -Dbuild.bin.emacs=" el-get-emacs " -Delib.dir= -Dcedet.dir=" el-get-dir "cedet -Ddist.dir=dist"))
               ;; :build ("touch `find . -name Makefile`" "make")
               :branch "bzr-cedet"
               :load-path ("dist/lisp"))
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
               :features (loc-changes load-relative test-simple)
               :url "https://github.com/jkl1337/emacs-dbgr")
        (:name pkgbuild-mode
               :lazy t)
        ))

(progn
  (setq jkl/el-get-packages
        '(el-get git-emacs fuzzy popup cedet escreen jdee auto-complete
                 pkgbuild-mode
                 flymake ; wow, the emacs one is a POS
                 ido-ubiquitous
                 color-theme
                 bbdb bbdb-vcard org-mode
                 rainbow-mode rainbow-delimiters
                 markdown-mode nxhtml org-mode pylookup python-mode pymacs lua-mode
                 flymake-easy
                 rcodetools rvm rdebug rinari rhtml-mode rspec-mode yari rsense ruby-block
                 yaml-mode haml-mode
                 emms xcscope git-blame slime yasnippet csharp-mode jquery-doc
                 html5 js2-mode multi-web-mode coffee-mode
                 sass-mode
                 magit
                 clojure-mode nrepl ac-nrepl paredit))

  (el-get 'sync jkl/el-get-packages))

(byte-recompile-directory (jkl/script-dir) 0)

(jkl/load-script "themes")
(condition-case nil
    (color-theme-cool-dark)
    (error nil))

;; Request to merge custom info.
;; Consider setting additional-path with default list in order to
;; have custom docs separated (put them in INFOPATH)
;; (add-to-list 'Info-default-directory-list 
;;              (expand-file-name jkl/info-path))

;;;; ELPA
;; package.el is carried in the contrib directory for now
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (package-initialize) 

;;;; CUSTOM KEY BINDINGS
(define-key jkl/func-map "c" 'recompile)

;; Flymake error nav
(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)

;;;; Command/environment customizations
;; Windows msys shell
(when jkl/mswinp
  (let ((msys-bash-exe (locate-file "bash.exe" exec-path)))
    (when (string-match-p "/MSYS/" (upcase msys-bash-exe))
      (jkl/custom-set 'shell-file-name (file-name-sans-extension msys-bash-exe))
      (jkl/custom-set 'explicit-shell-file-name shell-file-name)
      (jkl/custom-set 'explicit-bash-args '("--login" "--noediting" "-i"))
      (jkl/custom-set
       'shell-mode-hook '(lambda ()
                           (tabkey2-mode nil)
                           (ansi-color-for-comint-mode-on)
                           ;;(setq comint-scroll-show-maximum-output 'this)
                           (make-variable-buffer-local 'comint-completion-addsuffix)
                           (setq comint-completion-addsuffix t)
                           (setq w32-quote-process-args ?\"))))))

;; For non-Windows at least make sure we have ANSI escapes working
(unless jkl/mswinp
  (jkl/custom-set 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;; Going to try out Ctrl-Tab for a bit as a replacement for M-tab

(defvar jkl/mswin-C-tab-or-disable-alt-win t
  "If true, use C-tab as a replacement for M-tab, otherwise, let's
try disabling Alt-Tab switching and see how that works")

(when jkl/mswinp
  (if jkl/mswin-C-tab-or-disable-alt-win
      (define-key function-key-map [(control tab)] [?\M-\t])
    (w32-register-hot-key [M-tab])))

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

;;;; EMMS
;;; FIXME: Reconfigure EMMS

;;;; BBDB

;;;; ORG-MODE
(require 'org nil)

(jkl/custom-set 'major-mode 'text-mode)
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
;; (require 'yasnippet)
(add-to-list 'yas-snippet-dirs (concat jkl/my-dir "yasnippet"))
(yas-global-mode)

;;;; NXHTML
(jkl/custom-set 'mumamo-chunk-coloring 10)
;;(tabkey2-mode)

;;;; CEDET and ECB
;;; see cedet-setup.el

;;; ECB - Code Browser

;;;; AUTO-COMPLETE
(jkl/custom-set 'ac-quick-help-delay 1.0)

(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)

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
    (jkl/custom-set 'lua-default-application (expand-file-name jlua-sh)))
  (when (executable-find "luajit")
    (setenv "LUAI" "luajit")))

;;;; BEGIN CUSTOMIZATION

;;; TRAMP administer root files on remote hosts
(jkl/add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:luebsj@%h:"))
(jkl/add-to-list 'tramp-default-proxies-alist '((regexp-quote (system-name)) nil nil))

;;; GLOBAL settings
(find-function-setup-keys)
(define-key 'help-command "\C-f" 'find-function)
(define-key 'help-command "\C-v" 'find-variable)

(global-set-key "\C-c\C-d" (lambda ()
                              (interactive)
                              (let ((cur-window (selected-window)))
                                (other-window 1)
                                (quit-window)
                                (select-window cur-window))))

(global-set-key "\M-/" 'hippie-expand)

(jkl/custom-set 'fill-column 72)
;; going to go ahead and default to no tabs globally
(jkl/custom-set 'indent-tabs-mode nil)

;;; show-trailing-whitespace
(jkl/custom-set 'show-trailing-whitespace t)

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

(ido-mode 1)
(ido-ubiquitous-mode 1)
(ido-ubiquitous-disable-in execute-extended-command)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(show-paren-mode 1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(display-time-mode)
(jkl/custom-set 'display-time-24hr-format t)

(jkl/custom-set 'inhibit-startup-screen t
                'initial-scratch-message nil
                'auto-save-interval 3000
                'auto-save-timeout 300
                'make-backup-files nil)

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
(jkl/custom-set 'vc-p4-require-p4config t)
;; Damn, it's not working well
;; (jkl/add-to-list 'vc-handled-backends 'P4) 

;;; A find-file-hook that is interactive is arguably completely evil
;;; but perhaps I will assign this to a keybinding, later
;; (unless jkl/mswinp
;;   (add-hook 'find-file-hook 'jkl/remove-or-convert-trailing-ctl-M))

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
