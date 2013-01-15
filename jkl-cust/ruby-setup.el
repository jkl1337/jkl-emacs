
(defun jkl/ac-ruby-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-jkl-rcodetools))

(eval-after-load "auto-complete-config"
  '(progn
     (ac-define-prefix 'rct-prefix "\\(?:\\.\\|::\\)")

     (ac-define-source jkl-rcodetools
       (cons '(prefix . rct-prefix) ac-source-rcodetools))

     ;; Disable whatever comes with auto-complete for now
     (fset 'ac-ruby-mode-setup 'jkl/ac-ruby-mode-setup)))
