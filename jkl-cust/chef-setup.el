(defun chef-resource-lookup ()
  "Open the documentation in a browser for the chef resource at point"
  (interactive)
  (let* ((base "http://docs.opscode.com")
         (sym (symbol-at-point)))
    (other-window 1)
    (w3m-browse-url (format "%s/resource_%s.html" base sym))))

(define-key ruby-mode-map (kbd "C-h 3") 'chef-resource-lookup)
