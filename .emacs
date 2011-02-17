(setq x-super-keysym 'meta)
(setq x-hyper-keysym 'super)

(setq load-path (cons "~/emacs" load-path))

(setq load-path (cons "~/site-lisp/bbdb-vcard" load-path))
(require 'bbdb-vcard)

(tool-bar-mode -1)

;; JKL .emacs initialization file

;; org-mode stuff
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; nxhtml
(set 'nxhtml-path "/home/luebsj/site-lisp/nxhtml/")
(load (concat nxhtml-path "autostart.el"))

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

; (require 'un-define)
(font-lock-mode)
(global-font-lock-mode 1)

(global-set-key "\C-g" 'goto-line)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(autoload 'doc-mode "doc-mode")
(add-to-list 'auto-mode-alist '("\\.doc$" . doc-mode))

;; (require 'php-mode)
(autoload 'css-mode "css-mode")

(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)
;;
(add-hook 'php-mode-user-hook 'turn-on-font-lock)
;;
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

(set-face-background 'mmm-default-submode-face "gray15")

;;
;; set up an mmm group for fancy html editing
(mmm-add-group
 'fancy-html
 '(
   (html-php-tagged
    :submode php-mode
    :face mmm-code-submode-face
    :front "<[?]php"
    :back "[?]>")
   (html-css-attribute
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "style=\""
    :back "\"")))
;;
;; What files to invoke the new html-mode for?
(add-to-list 'auto-mode-alist '("\\.inc\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.[sj]?html?\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))
;;
;; What features should be turned on in this html-mode?
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil embedded-css))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-html))
;;
;; Not exactly related to editing HTML: enable editing help with mouse-3 in all sgml files
(defun go-bind-markup-menu-to-mouse3 ()
  (define-key sgml-mode-map [(down-mouse-3)] 'sgml-tags-menu))
;;
(add-hook 'sgml-mode-hook 'go-bind-markup-menu-to-mouse3)

(setq auto-mode-alist (append '(("\\.py$" . python-mode) 
				("\\.tex$" . latex-mode))
			      auto-mode-alist))

(setq prolog-program-name '((swi "swipl")))
(setq prolog-system 'swi)

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq c-basic-offset 8))


(defun hhp-c-mode ()
  "C mode with adjusted defaults for use with HHP."
  (interactive)
  (c-mode)
  (c-set-style "stroustrup")
  (setq c-basic-offset 4)
  (setq tab-width 4))

;; god help me
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" . 
				 visual-basic-mode)) auto-mode-alist))

;; Initialize Rope / Python

(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;;(autoload 'linux-c-mode "linux-c-mode" "Sane coding mode." t)
;;(setq auto-mode-alist (append '(("\\.[ch]$" .
;;				 linux-c-mode)) auto-mode-alist))

(autoload 'hhp-c-mode "hhp-c-mode" "Insane coding mode." t)
(setq auto-mode-alist (append '(("\\.\\(cpp\\|[ch]\\)$" .
				 hhp-c-mode)) auto-mode-alist))
      
(autoload 'mwheel-install "mwheel" "Enable Mouse Wheel support")
(mwheel-install)

;; -----------------------------------------------------------------
;; AUC TeX

;(add-to-list 'load-path "c:/program files/emacs/site-lisp/auctex/")
;;(add-to-list 'Info-default-directory-list "c:/program files/emacs/site-lisp/auctex/doc/")
;;(load "tex-site") 
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; -----------------------------------------------------------------
;; Reftex activation (Reftex is included with Emacs 21.1)

(autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

(set-face-foreground 'default  "green")
(set-face-background 'default  "black")
(set-face-foreground 'font-lock-type-face  "yellow")
(set-face-foreground 'font-lock-string-face  "orange")
(set-face-foreground 'font-lock-constant-face  "plum1")

(set-face-foreground 'font-lock-variable-name-face  "white")
(set-face-foreground 'font-lock-function-name-face  "cyan")
(set-face-foreground 'font-lock-keyword-face  "cyan")
(set-face-foreground 'font-lock-comment-face  "aquamarine")


;;(set-face-background 'default  "white")

;; (set-face-font 'default "-*-bitstream vera sans mono-medium-r-*-*-*-100-*-*-*-*-iso10646-1")
;;(set-face-font 'gui-element "-*-tahoma-medium-r-*-*-*-90-*-*-*-*-iso8859-1")
;;(set-face-font 'widget "-*-tahoma-medium-r-*-*-*-90-*-*-*-*-iso8859-1")

(setq auto-save-interval 3500)
(setq auto-save-timeout nil)

 (add-hook 'find-file-hooks 'remove-or-convert-trailing-ctl-M)
  (defun remove-or-convert-trailing-ctl-M ()
    "Propose to remove or convert trailing ^M from a file."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "\^M" nil t)
          ;; a ^M is found
          (if (or (= (preceding-char) ?\^J)
                  (= (following-char) ?\^J) )
              ;; Must find a way to display the buffer before this question
              (if (y-or-n-p "Remove trailing ^M ? ")
                  (progn (goto-char (point-min))
                         (perform-replace "\^M" "" nil nil nil)
                         (pop-mark) )
                (message "No transformation.") )
            (if (y-or-n-p "Convert ^M into ^J ? ")
                (progn (goto-char (point-min))
                       (perform-replace "\^M" "\^J" nil nil nil)
                       (pop-mark) )
              (message "No transformation.") ) )
        ;;(message "No ^M in this file !")
        ) ) )

;;--------------------------------------------------------------------
;; Lines enabling gnuplot-mode

;; move the files gnuplot.el to someplace in your lisp load-path or
;; use a line like
;;  (setq load-path (append (list "/path/to/gnuplot") load-path))

;; these lines enable the use of gnuplot mode
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; This line binds the function-9 key so that it opens a buffer into
;; gnuplot mode 
  (global-set-key [(f9)] 'gnuplot-make-buffer)


;; end of line for gnuplot-mode
;;--------------------------------------------------------------------
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mumamo-chunk-coloring 2)
 '(nxhtml-default-encoding (quote utf-8))
 '(org-agenda-files (quote ("~/org/curgtd.org")))
)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
