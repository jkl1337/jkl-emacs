
(defun jkl/add-exec-paths (&rest paths)
  "Append string arguments to PATH environment and emacs @exec-path."
  (while paths
    (let ((path (car paths))
	  (winp (string= system-type "windows-nt")))
      (setenv "PATH" (concat (getenv "PATH") 
			     (if winp ";" ":")
			     (if winp (replace-regexp-in-string "/" "\\\\" path) path)))
      (setq exec-path (nconc exec-path (list path))))
    (setq paths (cdr paths))))

(defun jkl/set-saved-vars (&rest args)
  "Set value of variables and notify customize."
  (while args
    (let ((entry (car args)))
      (let* ((symbol (indirect-variable (nth 0 entry)))
	     (value (nth 1 entry)))
	(set symbol value)
	(put symbol 'customized-value (list (custom-quote value)))
	;;(put symbol 'saved-value (list (custom-quote value)))
	)
      )
    (setq args (cdr args))))