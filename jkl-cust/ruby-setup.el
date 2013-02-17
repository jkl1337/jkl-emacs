
(eval-after-load 'ruby-mode
  '(progn
     (require 'flymake-easy)

     (defconst flymake-ruby-err-line-patterns
       '(("^\\(?:SyntaxError in \\)?\\(.*\.rb\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)))

     (defvar flymake-ruby-executable "ruby"
       "The ruby executable to use for syntax checking.")

     ;; Invoke ruby with '-c' to get syntax checking
     (defun flymake-ruby-command (filename)
       "Construct a command that flymake can use to check ruby source."
       (list flymake-ruby-executable "-w" "-c" filename))

;;;###autoload
     (defun flymake-ruby-load ()
       "Configure flymake mode to check the current buffer's ruby syntax."
       (interactive)
       (flymake-easy-load 'flymake-ruby-command
                          flymake-ruby-err-line-patterns
                          'tempdir
                          "rb"))

     (add-to-list 'flymake-allowed-file-name-masks  '(".+\\.rb$" nil))
     (add-to-list 'flymake-allowed-file-name-masks '("Rakefile$" nil))

     (defun flymake-ruby-maybe-enable ()
       (when (and buffer-file-name
                  (file-writable-p (file-name-directory buffer-file-name))
                  (file-writable-p buffer-file-name))
         (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
         (flymake-ruby-load)))

     (provide 'flymake-ruby)

     (add-hook 'ruby-mode-hook 'flymake-ruby-maybe-enable)))


(global-rinari-mode 1)

(eval-after-load 'ruby-mode
  '(progn
     (setq ruby-use-encoding-map nil)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)))

(jkl/custom-set 'rinari-major-modes '(dired-mode ruby-mode css-mode sass-mode
                                                 rhtml-mode
                                                 eruby-html-mumamo-mode eruby-nxhtml-mumamo-mode
                                                 coffee-mode js2-mode javascript-mode
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
  (delq nil (mapcar '(lambda(line)
                       (if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))
                    (split-string (shell-command-to-string "rake -T") "[\n]"))))

(defun rake (task)
  (interactive (list (completing-read "Rake (default: default): "
                                      (pcmpl-rake-tasks))))
  (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))


(eval-after-load 'ruby-mode
  '(progn
     (setq ruby-program "/usr/bin/ruby")

     (defun jkl/ruby-setup ()
       (require 'rsense)
       ;; the rsense "launcher" can only run with MRI ruby, because it has to fork
       ;; the proper config is handled by .rsense anyway
       (setq ac-sources
             (cons ac-source-rsense ac-sources)))
     ;; flymake-ruby-mode
     (add-hook 'ruby-mode-hook 'jkl/ruby-setup)))

;; got rid of rcodetools. It's in git history now
