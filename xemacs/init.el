(font-lock-mode)
;; AUC TeX
(load "tex-site")

(autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; Red Hat Linux default .emacs initialization file

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq c-basic-offset 8))

;; god help me
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" . 
				 visual-basic-mode)) auto-mode-alist))

(autoload 'linux-c-mode "linux-c-mode" "Sane coding mode." t)
(setq auto-mode-alist (append '(("\\.[ch]$" .
				 linux-c-mode)) auto-mode-alist))
      
(autoload 'mwheel-install "mwheel" "Enable Mouse Wheel support")
(mwheel-install)

;; #(require 'php-mode)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)


(set-face-background 'default  "white")
;; (set-face-font 'default "-*-courier new-medium-r-*-*-*-40-*-*-*-*-iso8859-*")


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

