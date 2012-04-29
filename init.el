;; TODO: change paren mode highlight faces
;; see face-user-default-spec in faces.el TODO

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

;;;; Misc 3rd party scripts
(let ((contrib-dir (concat jkl/my-dir "contrib/")))
  (jkl/load-path-add-immediate-subdirs contrib-dir)
  (add-to-list 'load-path contrib-dir))

;; load host specific early-init
(jkl/load-script (concat "host-"
			 (substring system-name 0 (string-match "\\." system-name))
			 ".el") t)

;;;; EL-GET

(jkl/custom-set 'el-get-dir (concat user-emacs-directory "el-get/"))
(jkl/custom-set 'el-get-git-shallow-clone t)

;;; Hacks
; https://github.com/dimitri/el-get/issues/529
(let ((cedet-dir (expand-file-name "cedet" el-get-dir)))
  (add-to-list 'load-path (expand-file-name
                           "common" cedet-dir))
  (add-to-list 'load-path (expand-file-name
                           "speedbar" cedet-dir))
  (require 'inversion nil t))

;; site CEDET early override
(let ((cedet-paths
       `(,(expand-file-name "cedet" (concat el-get-dir "cedet/common/"))
         "/usr/share/emacs/site-lisp/cedet/common/cedet")))
  (while cedet-paths
    (let ((pth (car cedet-paths)))
          (when (or
                 (file-exists-p (concat pth ".elc"))
                 (file-exists-p (concat pth ".el")))
            (load pth nil)))
    (setq cedet-paths (cdr cedet-paths))))

;;; Bootstrap el-get
(add-to-list 'load-path (concat el-get-dir "el-get"))
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (end-of-buffer)
       (eval-print-last-sexp))))
  (require 'el-get))

;;; el-get recipes
(setq el-get-sources
      '((:name jdee
               :website "http://jdee.sourceforge.net/"
               :description "The JDEE is an add-on software package that turns Emacs into a comprehensive system for creating, editing, debugging, and documenting Java applications."
               :type git
               :depends cedet
               :url "http://github.com/jkl1337/jdee.git"
               :build `(,(concat "ant bindist -Dbuild.bin.emacs=" el-get-emacs " -Delib.dir= -Dcedet.dir=" el-get-dir "cedet -Ddist.dir=dist"))
               ;; :build ("touch `find . -name Makefile`" "make")
               :load-path ("dist/lisp"))
        (:name pymacs
               :description "Interface between Emacs Lisp and Python"
               :type git
               :url "http://github.com/pinard/Pymacs.git"
               :post-init
               (lambda ()
                 ;; do PYTHONPATH=~/.emacs.d/el-get/pymacs/:$PYTHONPATH
                 (setenv
                  "PYTHONPATH"
                  (let ((pp (or (getenv "PYTHONPATH") "")))
                    (concat default-directory
                            (unless (string-prefix-p ":" pp) ":")
                            pp)))
                 (autoload 'pymacs-load "pymacs" nil t)
                 (autoload 'pymacs-eval "pymacs" nil t)
                 (autoload 'pymacs-exec "pymacs" nil t)
                 (autoload 'pymacs-call "pymacs")
                 (autoload 'pymacs-apply "pymacs"))
               :build `(,(concat "make"
                                 (if (file-executable-p "/usr/bin/python2")
                                     " PYTHON=python2" " "))))
        (:name bbdb
               :website "http://bbdb.sourceforge.net/"
               :description "The Insidious Big Brother Database (BBDB) is a contact management utility."
               :type git
               :url "git://git.savannah.nongnu.org/bbdb.git"
               :load-path ("./lisp")
               ;; if using vm, add `--with-vm-dir=DIR' after ./configure
               :build `("autoconf" ,(concat "./configure --with-texmf-dir=/usr/share/texmf --with-emacs=" el-get-emacs) "make bbdb")
               :features bbdb-loaddefs
               :autoloads nil
               :post-init (progn (bbdb-initialize)))
        (:name ecb
               :description "Emacs Code Browser"
               :type cvs
               :depends cedet
               :module "ecb"
               :url ":pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb"
               :build `(("make" "CEDET=" ,(concat (shell-quote-argument el-get-dir) "cedet/common/")
                         ,(concat "EMACS=" (shell-quote-argument el-get-emacs)))))
        (:name python-mode
               :type git
               :url "https://github.com/jkl1337/python-mode.git"
               :description "Major mode for editing Python programs"
               :features (python-mode doctest-mode)
               :depends highlight-indentation
               :compile ("python-mode.el" "python-extended-executes.el" "test/doctest-mode.el")
               :load "test/doctest-mode.el"
               :prepare (progn
                          (autoload 'python-mode "python-mode"
                            "Python editing mode." t)
                          (add-to-list 'auto-mode-alist
                                       '("\\.py$" . python-mode))
                          (add-to-list 'interpreter-mode-alist
                                       '("python" . python-mode))))

        (:name lua-mode
               :description "A major-mode for editing Lua scripts"
               :website "https://github.com/immerrr/lua-mode"
               :description "A major mode for editing Lua scripts."
               :type git
               :branch "compile-fix"
               :url "https://github.com/jkl1337/lua-mode")
        (:name csharp-mode
               :website "https://code.google.com/p/csharpmode/"
               :description "This is a mode for editing C# in emacs. It's based on cc-mode, v5.30.3 and above."
               :type git
               :url "luebsj@luebsphoto.com:/srv/git/csharp-mode.git"
               :features csharp-mode)
        ;; stupid org-mode git broke
        ;; (:name org-mode
        ;;        :website "http://orgmode.org/"
        ;;        :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system."
        ;;        :type git
        ;;        :checkout "release_7.8.07"
        ;;        :url "git://orgmode.org/org-mode.git"
        ;;        :info "doc"
        ;;        :build `,(mapcar
        ;;                   (lambda (target)
        ;;                     (list "make" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
        ;;                   '("clean" "all"))
        ;;        :load-path ("." "lisp" "contrib/lisp")
        ;;        :autoloads nil
        ;;        :features org-install)
        ))

(setq jkl/el-get-packages
 '(el-get git-emacs fuzzy popup cedet ecb escreen jdee auto-complete
   nxhtml org-mode pylookup python-mode pymacs lua-mode emms xcscope
   git-blame slime yasnippet csharp-mode bbdb bbdb-vcard jquery-doc
   html5))

(el-get 'sync jkl/el-get-packages)

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

;;;; w3m
(require 'w3m-load nil t)
(when (and (not jkl/mswinp) (featurep 'w3m-load))
  (jkl/custom-set
   'browse-url-browser-function
   (nconc (list 
           '("file:.*/pylookup/python-2" . w3m-browse-url)
           '("http://.*docs.python.org/". w3m-browse-url)
           '("file:.*/usr/local/share/gtk-doc/html" . w3m-browse-url)
           '("file:.*/usr/share/gtk-doc/html" . w3m-browse-url)
           '("file:.*/usr/share/devhelp/books" . w3m-browse-url))
          (if (listp browse-url-browser-function)
              browse-url-browser-function
            `(("." . ,browse-url-browser-function))))))

;;;; EMMS
(let ((emms-lisp-dir (concat jkl/my-dir "contrib/emms/lisp")))
  (when (file-readable-p (concat emms-lisp-dir "/emms-setup.elc"))
    (add-to-list 'load-path emms-lisp-dir)
    (require 'emms-setup)
    (emms-standard)
    (emms-default-players)
    ;; (define-emms-simple-player mpg123 '(file url)
    ;;   (emms-player-simple-regexp "mp3" "mp2")
    ;;   "mpg123")
    ;; (jkl/custom-set 'emms-player-list (cons 'emms-player-mpg123 emms-player-list))
    (add-to-list 'Info-default-directory-list (expand-file-name (concat emms-lisp-dir "/../doc")))))

;;;; BBDB

;;;; ORG-MODE
(when (car (jkl/try-add-pkg "org-mode/lisp" "org-mode/contrib/lisp"))
  (let ((org-info-dir (concat jkl/pkg-path "org-mode/doc")))
    (when (file-readable-p (concat org-info-dir "/dir"))
      (add-to-list 'Info-default-directory-list (expand-file-name org-info-dir)))))

(require 'org-install nil)

(jkl/custom-set 'major-mode 'text-mode)
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

;; BEGIN APPEARANCE / BASIC FACES
(when jkl/mswinp
  (jkl/set-face 'default '((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil
                               :overline nil :underline nil :background "black" :foreground "green" 
                               :slant normal :weight normal :height 90 :width normal
                               :foundry "*" :family "Lucida Console")))))

(unless jkl/mswinp
  (let ((font-param))
    ;;(setq font-param '("ProggyCleanTT" . 120))
    (setq font-param '("Terminus" . 100))
    (jkl/set-face 'default `((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil
                                 :overline nil :underline nil :background "black" :foreground "green" 
                                 :slant normal :weight normal :height ,(cdr font-param) :width normal
                                 :foundry "*" :family ,(car font-param)))))))

(jkl/custom-set 'default-frame-alist
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

;;;; GTAGS
(autoload 'gtags-mode "gtags" "" t)

;;;; LISP / SLIME
(slime-setup '(inferior-slime))

;;;; YASNIPPET
;; (require 'yasnippet)
(yas/global-mode t)

;;;; NXHTML
(tabkey2-mode)

;;;; CEDET and ECB

;;; ECB - Code Browser

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
;;; GLOBAL settings
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
(add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))

;; arch and pacman PKGBUILD files
(add-to-list 'auto-mode-alist (cons "PKGBUILD$"
                                    #'(lambda ()
                                        (sh-mode)
                                        (sh-set-shell "bash" t))))

(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))

(global-set-key (kbd "M-w") 'copy-region-as-kill)

(iswitchb-mode 1)
(show-paren-mode 1)
;;(transient-mark-mode -1)
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
             (define-key emacs-lisp-mode-map [f5] 'eval-region)
             (define-key emacs-lisp-mode-map (kbd "C-c <f5>") 'eval-buffer)))

(add-hook 'emacs-lisp-mode-hook 'jkl/remove-elc-on-save)

;;;; PERFORCE
;; Adding this backend causes indefinite wait in all non-rev controlled directories
(jkl/add-to-list 'vc-handled-backends 'P4)

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
 "clisp-setup.el"
 )

(setq custom-file (concat jkl/my-dir "custom.el"))
(load custom-file)

(server-start)
