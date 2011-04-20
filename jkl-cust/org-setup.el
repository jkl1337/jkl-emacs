;; ORG-MODE enable yas/flyspell
(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 78)
            (auto-fill-mode 1)
            (flyspell-mode 1)))

(defun jkl/lst-org-files (dir)
  (if (file-directory-p dir)
      (append
       (directory-files dir t ".org$")
       (apply 'append (mapcar 'jkl/lst-org-files (directory-files dir t "[^.]+"))))))

(jkl/custom-set 'org-agenda-files
                (jkl/lst-org-files (concat jkl/git-repo-dir "jkl-org")))

(jkl/custom-set
 'org-fast-tag-selection-single-key t
 'org-treat-S-cursor-todo-selection-as-state-change nil
 'org-todo-keywords '((sequence "TODO(t!)" "STARTED(s!)" "WAITING(w@/!)" "APPT(a)" "MAYBE(m!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)" "DEFFERED(f@/!)"))
 'org-log-done 'time
 'org-log-into-drawer t)

(setq org-capture-templates '(("t" "todo" entry (file org-default-notes-file) "* TODO %?\n%U\n%a" :clock-in t :clock-resume t)
                              ("n" "note" entry (file org-default-notes-file) "* %? :NOTE:\n%U\n%a\n:CLOCK:\n:END:" :clock-in t :clock-resume t)
                              ("a" "appointment" entry (file+datetree jkl/org-diary-file) "* %?
%U" :clock-in t :clock-resume t)
                              ("p" "Phone call" entry (file org-default-notes-file) "* Phone %(jkl/phone-call) - %(gjg/bbdb-company) :PHONE:\n%U\n\n%?"
                               :clock-in t :clock-resume t)
                              ("w" "org-protocol" entry (file org-default-notes-file) "* TODO Review %c\n%U" :immediate-finish t :clock-in t :clock-resume t)))


(org-clock-persistence-insinuate)
;; (setq org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK"))
;; (setq org-clock-into-drawer "CLOCK")
;; (setq org-clock-out-remove-zero-time-clocks t)
;; (setq org-clock-out-when-done t)
 
(defun gtd ()
   (interactive)
   (find-file "~/org/gtd.org")
)