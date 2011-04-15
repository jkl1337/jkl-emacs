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
