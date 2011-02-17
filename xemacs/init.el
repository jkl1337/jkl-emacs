;; Red Hat Linux default .emacs initialization file

(font-lock-mode)

;; give me C-DEL
(global-set-key '(control delete) 'delete-primary-selection)

;; prolog, not perl
;;(setq auto-mode-alist (append 
;;		       '(("\\.pl$" . prolog-mode) ("\\.tex$" . latex-mode)) 
;;		       auto-mode-alist))

(setq prolog-consult-string "[user].\n")

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.

;; god help me
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
;(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" . 
;				 visual-basic-mode)) auto-mode-alist))
      
(autoload 'mwheel-install "mwheel" "Enable Mouse Wheel support")
(mwheel-install)

;; AUC TeX
(load "tex-site")

;; #(require 'php-mode)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)


(set-face-foreground 'default  "white")
(set-face-background 'default  "black")
(set-face-foreground 'font-lock-type-face  "yellow")
(set-face-background 'font-lock-type-face  "black")
(set-face-foreground 'font-lock-string-face  "orange")
(set-face-background 'font-lock-string-face  "black")
(set-face-foreground 'font-lock-preprocessor-face  "plum1")
(set-face-background 'font-lock-preprocessor-face  "black")
(set-face-foreground 'font-lock-variable-name-face  "white")
(set-face-background 'font-lock-variable-name-face  "black")
(set-face-foreground 'font-lock-function-name-face  "cyan")
(set-face-background 'font-lock-function-name-face  "black")
(set-face-foreground 'font-lock-keyword-face  "cyan")
(set-face-background 'font-lock-keyword-face  "black")

;; (set-face-font 'default "-misc-fixed-medium-r-normal-*-*-120-*-*-c-*-iso8859-1")

(setq auto-save-interval 3500)
(setq auto-save-timeout nil)

; (add-hook 'find-file-hooks 'remove-or-convert-trailing-ctl-M)
;  (defun remove-or-convert-trailing-ctl-M ()
;    "Propose to remove or convert trailing ^M from a file."
;    (interactive)
;    (save-excursion
;      (goto-char (point-min))
;      (if (search-forward "\^M" nil t)
;          ;; a ^M is found
;          (if (or (= (preceding-char) ?\^J)
;                  (= (following-char) ?\^J) )
;              ;; Must find a way to display the buffer before this question
;              (if (y-or-n-p "Remove trailing ^M ? ")
;                  (progn (goto-char (point-min))
;                         (perform-replace "\^M" "" nil nil nil)
;                         (pop-mark) )
;                (message "No transformation.") )
;            (if (y-or-n-p "Convert ^M into ^J ? ")
;                (progn (goto-char (point-min))
;                       (perform-replace "\^M" "\^J" nil nil nil)
;                       (pop-mark) )
;              (message "No transformation.") ) )
;        ;;(message "No ^M in this file !")
;        ) ) )

