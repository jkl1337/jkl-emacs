(defun jkl/ac-ruby-mode-setup ()
  (require 'rcodetools)
  (add-to-list 'ac-sources 'ac-source-jkl-rcodetools))

(defun ac-prefix-rct-prefix ()
  "Ruby prefix."
  (if (re-search-backward "\\(?:\\.\\|::\\)\\(\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
      (match-beginning 1)))

(defun ac-rct-init ()
  (require 'rcodetools)
  (condition-case x
      (save-excursion
        (rct-exec-and-eval rct-complete-command-name "--completion-emacs-icicles"))
    (error) (setq rct-method-completion-table nil)))

(defun ac-rct-candidates ()
  (mapcar
    (lambda (completion)
      (replace-regexp-in-string "\t.*$" "" (car completion)))
    rct-method-completion-table))

(eval-after-load "auto-complete"
  '(progn
     (ac-define-prefix 'rct-prefix 'ac-prefix-rct-prefix)

     (ac-define-source jkl-rcodetools
       '((prefix . rct-prefix)
         (init . ac-rct-init)
         (candidates . ac-rct-candidates)
         (requires . 1)))

     (add-hook 'ruby-mode-hook 'jkl/ac-ruby-mode-setup)))
