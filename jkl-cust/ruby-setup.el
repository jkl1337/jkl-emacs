
(when (executable-find "rbenv")
  (global-rbenv-mode 1))

(defun jruby-dev ()
  (interactive)
  (let ((jruby-opts (getenv "JRUBY_OPTS"))
        (opts "-J-XX:+TieredCompilation -J-XX:TieredStopAtLevel=1"))
    (unless (and jruby-opts (string-match (regexp-quote opts) jruby-opts))
      (setenv "JRUBY_OPTS" (concat jruby-opts " " opts)))))

(defun jruby-toggle-os ()
  (interactive)
  (let* ((jruby-opts (getenv "JRUBY_OPTS"))
        (opts "-X+O")
        (rx-opts (regexp-quote opts)))
    (if (and jruby-opts (string-match rx-opts jruby-opts))
      (setenv "JRUBY_OPTS" (replace-regexp-in-string rx-opts " " jruby-opts))
      (setenv "JRUBY_OPTS" (mapconcat 'identity (list jruby-opts opts) " ")))))


(global-rinari-mode 1)

(eval-after-load 'rinari
  '(progn
     (defadvice rinari-minor-mode (after rinari-on-enter activate)
       "Activate rails mode for yasnippet expansion"
       (when (or (and (integerp ad-return-value) (> ad-return-value 0))
                 ad-return-value)
         (add-to-list (make-local-variable 'yas-extra-modes) 'rails-mode)))))

(eval-after-load 'ruby-mode
  '(progn
     (setq ruby-deep-indent-paren nil)
     (setq ruby-use-encoding-map nil)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)))

(jkl/cs 'rinari-major-modes '(dired-mode ruby-mode css-mode sass-mode
                                         rhtml-mode
                                         eruby-html-mumamo-mode eruby-nxhtml-mumamo-mode
                                         coffee-mode js2-mode js3-mode javascript-mode
                                         yaml-mode
                                         dired-mode))

(define-key 'help-command (kbd "C-r") 'yari)

(add-to-list 'auto-mode-alist '("\\.\\(?:rake\\|thor\\|gemspec\\|ru\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(?:Rakefile\\|Throrfile\\|Cheffile\\|Gemfile\\|Capfile\\|Vagrantfile\\)\\'" . ruby-mode))
(add-to-list 'completion-ignored-extensions ".rbc")
(add-to-list 'completion-ignored-extensions ".rbo")

 ;; Clear the compilation buffer between test runs.
(eval-after-load 'ruby-compilation
  '(progn
     (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
       (let ((comp-buffer-name (format "*%s*" name)))
         (when (get-buffer comp-buffer-name)
           (with-current-buffer comp-buffer-name
             (delete-region (point-min) (point-max))))))
     (ad-activate 'ruby-do-run-w/compilation)))

;;; Rake

(defun pcomplete/rake ()
  "Completion rules for the `ssh' command."
  (pcomplete-here (pcmpl-rake-tasks)))

(defun pcmpl-rake-tasks ()
  "Return a list of all the rake tasks defined in the current
projects. I know this is a hack to put all the logic in the
exec-to-string command, but it works and seems fast"
  (delq nil (mapcar #'(lambda(line)
                       (if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))
                    (split-string (shell-command-to-string "rake -T") "[\n]"))))

(defun rake (task)
  (interactive (list (completing-read "Rake (default: default): "
                                      (pcmpl-rake-tasks))))
  (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))


(eval-after-load 'ruby-mode
  '(progn
     (setq ruby-program "/usr/bin/ruby")

     (add-hook 'ruby-mode-hook 'ruby-tools-mode)
     (add-hook 'ruby-mode-hook 'robe-mode)
     (defun jkl/ruby-setup ()
       (when (require 'ruby-block nil t)
         (ruby-block-mode 1))
       (add-to-list (make-local-variable 'company-backends) 'company-robe))
     (add-hook 'ruby-mode-hook 'jkl/ruby-setup)))

(require 'url)

(defun search-site-url (site url keyword)
  (concat "http://www.google.com/"
          (format "search?q=%s+site:%s+inurl:%s&btnI"
                  (url-hexify-string keyword)
                  (url-hexify-string site)
                  (url-hexify-string url))))

(defun ruby-help ()
  "Open ruby documentation for word under the point"
  (interactive)
  ;; (let ((w3m-pop-up-windows t))
  ;;   (when (one-window-p) (split-window))
  ;;   (other-window 1)
    (browse-url (search-site-url  "ruby-doc.org" "/" 
                                      (thing-at-point 'symbol))))
(define-key ruby-mode-map (kbd "C-h C-a") 'ruby-help)

(require 'ruby-additional)
