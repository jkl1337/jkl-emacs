(eval-after-load "scss-mode"
  ;; For now I don't use, and this prevents annoying warnings
  '(progn
     (setq scss-compile-at-save nil)))

(eval-after-load "coffee-mode"
  '(progn
     (let ((map coffee-mode-map))
       (mapcan (lambda (ks)
                 (define-key map (read-kbd-macro ks) nil))
               '("A-r" "A-R" "A-M-r"))
       (define-key map (kbd "s-r") 'coffee-compile-buffer)
       (define-key map (kbd "s-R") 'coffee-compile-region)
       (define-key map (kbd "s-M-r") 'coffee-repl))
     (jkl/cs 'coffee-tab-width 2)))

(eval-after-load "js3-mode"
  '(progn
     (set-default 'js3-indent-on-enter-key t)
     (set-default 'js3-mirror-mode t)
     (set-default 'js3-enter-indents-newline t)))

(eval-after-load "js2-mode"
  '(progn
     (set-default 'js2-basic-offset 4)))

(when (require 'multi-web-mode nil t)
  (setq mweb-default-major-mode 'html-mode)
  ;; (setq mweb-default-major-mode 'nxml-mode)
  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  (multi-web-global-mode 1)

  (add-hook 'nxml-mode-hook (lambda ()
                              (add-to-list (make-local-variable 'yas-extra-modes) 'html-mode))))








