(eval-after-load "coffee-mode"
  '(progn
     (let ((map coffee-mode-map))
       (mapcan (lambda (ks)
                 (define-key map (read-kbd-macro ks) nil))
               '("A-r" "A-R" "A-M-r"))
       (define-key map (kbd "s-r") 'coffee-compile-buffer)
       (define-key map (kbd "s-R") 'coffee-compile-region)
       (define-key map (kbd "s-M-r") 'coffee-repl))
     (jkl/custom-set 'coffee-tab-width 2)))

(eval-after-load "js2-mode"
  '(progn
     (add-hook 'js2-post-parse-callbacks 'jkl-js2-parse-global-vars-decls)

     (defun jkl-js2-parse-global-vars-decls ()
       (let ((btext (replace-regexp-in-string
                     ": *true" " "
                     (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
         (setq js2-additional-externs
               (split-string
                (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                " *, *" t))
         ))))

(when (require 'multi-web-mode nil t)
  (setq mweb-default-major-mode 'nxml-mode)
  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  (multi-web-global-mode 1)

  (add-hook 'nxml-mode-hook (lambda ()
                              (add-to-list (make-local-variable 'yas-extra-modes) 'html-mode))))
