(defun py-prev-def-name ()
  (save-excursion
    (if (re-search-backward "def[\t ]+\\([a-zA-Z0-9_]+\\)(" nil t)
        (match-string 1))))

(defun py-prev-def-args ()
  (save-excursion
    (if (re-search-backward "def[\t ]+\\([a-zA-Z0-9_]+\\)([\t ]*self[\t ]*,?[\t ]*\\(.*\\))" nil t)
        (match-string 2))))

(defun py-prev-class-name ()
  (save-excursion
    (if (re-search-backward "class[\t ]+\\([a-zA-Z0-9_]+\\)[ \t]*[(:]" nil t)
        (match-string 1))))
