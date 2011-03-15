
(defun jkl/load-path-add-immediate-subdirs (top-dir)
  (let* ((contents (directory-files top-dir))
	 (default-directory top-dir))
    (dolist (ent contents)
      (unless (member ent '("." ".." "RCS" "CVS" "rcs" "cvs"))
	(when (and (string-match "\\`[[:alnum:]]" ent)
		   (not (string-match "\\.elc?\\'" ent))
		   (file-accessible-directory-p ent))
	  (let ((expanded (expand-file-name ent)))
	    (unless (file-exists-p (expand-file-name ".nosearch"
						     expanded))
	      (add-to-list 'load-path expanded))))))))
		   
(defun jkl/add-exec-paths (&rest paths)
  "Append string arguments to PATH environment and emacs @exec-path."
  (while paths
    (let ((path (car paths)))
      (setenv "PATH" (concat (getenv "PATH")
			     path-separator
			     (convert-standard-filename path)))
      (setq exec-path (nconc exec-path (list path))))
    (setq paths (cdr paths))))

(defun jkl/try-add-load-path (&rest paths)
  "Try to add a directory to the head of the load-path using
add-to-list if it is accessible. A list of the directories that exist
is returned (whether added or not), with nil returned for each
directory that does not exist."
  (mapcar (lambda (dir)
	    (when (file-accessible-directory-p dir)
	      (add-to-list 'load-path dir)
	      dir))
	  paths))

(defun jkl/try-add-pkg (&rest pkg-dirs)
  (apply #'jkl/try-add-load-path
	 (mapcar (lambda (dir)
		   (concat jkl/pkg-path dir))
		 pkg-dirs)))

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
	    (value (nth 1 entry))
	    set)
	(setq set (or (get symbol 'custom-set) 'custom-set-default))
	(put symbol 'customized-value (list value))
	(funcall set symbol (eval valuel))
	)
      )
    (setq args (cdr args))))

(defun jkl/add-to-list (list-var elt &optional append compare-fn)
  (add-to-list list-var elt append compare-fn)
  (put list-var 'customized-value (list (custom-quote (eval list-var)))))

(defun jkl/set-face (&rest args)
  (while args
    (let ((face (car args))
	  (spec (cadr args)))
      ;;(put face 'customized-face spec)
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
