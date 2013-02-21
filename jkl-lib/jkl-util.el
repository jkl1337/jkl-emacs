(eval-when-compile (require 'cl))

(defun jkl/path-or-nil (path)
  "Return PATH if path exists, otherwise nil"
  (when (file-exists-p path) path))

(defun jkl/recompile-elc-on-save ()
  "Hooks the `after-save-hook' to delete elc files.
If there is an error in the recompile the file will have to be
manually forced recompiles."
  (add-hook 'after-save-hook
            (lambda ()
	      (let ((elc-file (concat buffer-file-name "c")))
		(when (file-exists-p elc-file)
		  (delete-file elc-file)
		  (condition-case nil
		      (byte-compile-file buffer-file-name)
		       (error (warn "%s failed to byte recompile" buffer-file-name))))))
            nil t))

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

(defun jkl/add-exec-path (path &optional prepend)
  "Append string argument path to PATH environment and emacs @exec-path."
  (let ((a (getenv "PATH"))
        (b (directory-file-name (convert-standard-filename path))))
    (when prepend
      (let (tmp)
        (setq tmp b b a a tmp)))
    (setenv "PATH" (concat a path-separator b)))
  (add-to-list 'exec-path path (not prepend)))

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

(defun jkl/set-internal (setter args)
  (let ((val))
    (while args
      (let ((sym (car args)))
	(unless (symbolp sym) (error "not symbolp: %s" sym))
        (setq val (cadr args))
        (funcall setter sym val)
        (put sym 'customized-value (list (custom-quote val))))
      (setq args (cddr args)))
    val))

(defun jkl/default-set (&rest args)
  "Set symbols similar to \\[set] but with multiple assignments and
notify customize. Note that this will NOT use the custom-set property
function, which may cause problems for some variables. @jkl/set-vars
will use custom-set when available."
  (jkl/set-internal 'set-default args))

(defun jkl/custom-set (&rest args)
  "Set symbols similar via set-default or uses custom-set property but
with multiple assignments and notify customize."
  (jkl/set-internal
   #'(lambda (sym val)
       (funcall
        (or (get sym 'custom-set) 'custom-set-default)
        sym val))
   args))

(defalias 'jkl/cs 'jkl/custom-set)

(defun jkl/cust-vars (&rest args)
  "Set quoted value of variables using the same method setup for
customize, using the custom-set property of the symbol, if available."
  (while args
    (let ((entry (car args)))
      (let ((symbol (indirect-variable (nth 0 entry)))
            (value (nth 1 entry))
            set)
        (setq set (or (get symbol 'custom-set) 'custom-set-default))
        (funcall set symbol (eval value))
        (put symbol 'customized-value (list value))))
    (setq args (cdr args))))

(defun jkl/add-to-list (list-var elt &optional append compare-fn)
  "Set value of LIST-VAR similar to add-to-list but also update
CUSTOMIZED-VALUE property"
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
        (funcall (car attr-funcs) face color)))))

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

(defun jkl/define-mode-keys (keymap key-defs)
  "Define a list of keys for keymap"
  (dolist (key-def key-defs)
    (define-key keymap (car key-def) (cdr key-def))))

(provide 'jkl-util)
