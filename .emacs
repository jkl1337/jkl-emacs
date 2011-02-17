

(if (string= system-type "windows-nt")
    (setq jkl/default-font-spec '(:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :family "Lucida Sans Typewriter" :foundry "outline" :foreground "green" :background "black"))
  (setq jkl/default-font-spec '(:height 100 :family "Lucida Sans Typewriter")))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 `(default ((t ,jkl/default-font-spec))))

(defun jkl/add-path (path)
  (setenv "PATH" (concat (getenv "PATH") 
                         (if (string= system-type "windows-nt") ";" ":")
                         (replace-regexp-in-string "/" "\\\\" path)))
  (setq exec-path (nconc exec-path (list path)))
  )

;; additional tool paths for windows
(if (string= system-type "windows-nt")
    (progn
      (setq jkl/site-lisp-path "c:/prp/emacs-23.2/site-lisp/")
      (mapc 'jkl/add-path '("C:/prp/Python27"
                           "C:/MingW/msys/1.0/bin"
                           "C:/prp/Mercurial"
                           "C:/prp/Aspell/bin"))
      (setq ispell-program-name "aspell")
      )
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
)

(setq nxhtml-path (concat jkl/site-lisp-path "nxhtml/"))

;; Image is everything
(defun jkl/set-faces  (fl)
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

(jkl/set-faces 
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
(setq load-path (cons (concat jkl/site-lisp-path "org-mode/lisp")
                      (cons (concat jkl/site-lisp-path "org-mode/contrib/lisp") load-path)))
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
            (flyspell-mode 1)
            (auto-fill-mode 1)))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("c:/prp/project/simple.org")))
 '(org-fast-tag-selection-single-key t)
 '(org-treat-S-cursor-todo-selection-as-state-change t)
 '(org-todo-keywords (quote ((sequence "TODO(t!)" "STARTED(s!)" "WAITING(w@/!)" "APPT(a)" "MAYBE(m!)" "|" "DONE(d!/!) CANCELLED(c@/!) DEFFERED(f@/!)")))))

(defun jkl/clock-in-to-next (kw)
  "Switch task from TODO to NEXT when clocking in, skipping capture tasks
and tasks with subtasks"
  (if (and (string-equal kw "TODO")
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (has-subtask nil))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-not-done-keywords)
              (setq has-subtask t))))
        (when (not has-subtask)
          "NEXT"))))

(defvar gjg/capture-phone-record nil
  "Either BBDB record vector, or person's name as a string, or nil")

(defun gjg/bbdb-company ()
  "Return company of saved bbdb record, or empty string - for use in Capture templates"
  (if (and gjg/capture-phone-record (vectorp gjg/capture-phone-record))
      (or (bbdb-record-company gjg/capture-phone-record) "")
    "COMPANY"))

(defun gjg/bbdb-name ()
  "Return full name of saved bbdb record, or empty string - for use in Capture templates"
  (if (and gjg/capture-phone-record (vectorp gjg/capture-phone-record))
      (concat "[[bbdb:"
              (bbdb-record-name gjg/capture-phone-record) "]["
              (bbdb-record-name gjg/capture-phone-record) "]]")
    "NAME"))

(defun jkl/phone-call ()
  (interactive)
  (let* ((myname (completing-read "Who is calling? " (bbdb-hashtable) 'bbdb-completion-predicate 'confirm))
         (my-bbdb-name (if (> (length myname) 0) myname nil)))
    (setq gjg/capture-phone-record
          (if my-bbdb-name
              (first (or (bbdb-search (bbdb-records) my-bbdb-name nil nil)
                         (bbdb-search (bbdb-records) nil my-bbdb-name nil)))
            myname)))
  (gjg/bbdb-name))

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
  (yas/load-directory (concat jkl/site-lisp-path "yasnippet-0.6.1c/snippets"))
  )

;; NXHTML
(load (concat nxhtml-path "autostart.el"))
(tabkey2-mode t)

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

(unless (string= system-type "windows-nt") 
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
