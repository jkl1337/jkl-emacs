
(defun jkl/add-exec-paths (&rest paths)
  "Append string arguments to PATH environment and emacs @exec-path."
  (while paths
    (let ((path (car paths)))
      (setenv "PATH" (concat (getenv "PATH") 
			     (if (eq system-type 'windows-nt) ";" ":")
			     (convert-standard-filename path)))
      (setq exec-path (nconc exec-path (list path))))
    (setq paths (cdr paths))))

(defun jkl/setv (&rest args)
  "Set symbols similar to \\[set] but with multiple assignments and notify customize."
  (let ((val))
    (while args
      (let ((sym (car args)))
	(setq val (cadr args))
	(set-default sym val)
	(put sym 'customized-value (list (custom-quote val))))
      (setq args (cddr args)))
    val))

(defun jkl/set-vars (&rest args)
  "Set quoted value of variables and notify customize."
  (while args
    (let ((entry (car args)))
      (let ((symbol (indirect-variable (nth 0 entry)))
	    (value (nth 1 entry)))
	(set-default symbol (eval value))
	(put symbol 'customized-value (list value))
	;;(put symbol 'saved-value (list (custom-quote value)))
	)
      )
    (setq args (cdr args))))

(defun jkl/add-elt (list-var elt)
  ;; FIXME: need to use add-to-list and check identity and then update var.
  (jkl/setv list-var (cons elt (symbol-value sym))))

(defun jkl/set-face (&rest args)
  (while args
    (let ((face (car args))
	  (spec (cadr args)))
      ;(put face 'customized-face spec)
      (face-spec-set face spec))
    (setq args (cddr args))))

(defun jkl/set-face-colors (fl)
  (dolist (faceelt fl)
    (let ((attr-funcs '(set-face-foreground set-face-background))
	  (face (car faceelt)))
      (dolist (color (cdr faceelt))
	(funcall (car attr-funcs) face color)
	(setq sfunc (cdr attr-funcs))	   
	)
      )
    ))

(defun jkl/remove-or-convert-trailing-ctl-M ()
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

(provide 'jkl-util)
