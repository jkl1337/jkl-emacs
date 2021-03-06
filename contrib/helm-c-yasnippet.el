;;; helm-c-yasnippet.el --- helm source for yasnippet.el

;; Copyright (C) 2008, 2009, 2010, 2011 Kenji.I (Kenji Imakado) <ken.imakaado@gmail.com>
;; Copyright (C) 2012 Yuhei Maeda <yuhei.maeda_at_gmail.com>

;; Author: Kenji.I (Kenji Imakado) <ken.imakaado@gmail.com>
;; Version: 0.6.3
;; Package-version: 0.6.3
;; Package-Requires: ()
;; Package-Requires: ((helm "20120811")(yasnippet "20120822"))
;; Keywords: convenience, emulation

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Actions: Insert snippet, Open snippet file, Open snippet file other window
;; C-z: execute-persistent-action

;;; Changelog:
;;  2012/08/11   Port to helm 
;;               Fixed bug on yasnippet 0.7.0 
;;  2012/08/23   Support yasnippet 0.8.0  
;;               

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `helm-c-yas-complete'
;;    List of yasnippet snippets using `helm' interface.
;;  `helm-c-yas-create-snippet-on-region'
;;    Create a snippet from region.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `helm-c-yas-not-display-dups'
;;    if non-nil not display duplicate snippet otherwise display all snippet
;;    default = t
;;  `helm-c-yas-display-msg-after-complete'
;;    if non-nil display snippet key message in minibuffer after Complete
;;    default = t
;;  `helm-c-yas-snippets-dir-list'
;;    list of directory used to find snippet file
;;    default = nil
;;  `helm-c-yas-space-match-any-greedy'
;;    if non-nil helm pattern space match anyword greedy.
;;    default = nil
;;  `helm-c-yas-display-key-on-candidate'
;;    if non-nil helm display candidate(snippet name) include key
;;    default = nil

;; here's my yasnippet's configuration
;; (require 'yasnippet)
;; (require 'helm-c-yasnippet)
;; (setq helm-c-yas-space-match-any-greedy t) ;[default: nil]
;; (global-set-key (kbd "C-c y") 'helm-c-yas-complete)
;; (yas--initialize)
;; (yas-load-directory "<path>/<to>/snippets/")
;; (add-to-list 'yas-extra-mode-hooks 'ruby-mode-hook)
;; (add-to-list 'yas-extra-mode-hooks 'cperl-mode-hook)

(eval-when-compile
  (require 'cl))
(require 'helm)
(require 'yasnippet)

;;; Code
(defvar helm-c-yas-version "0.6" "Version of helm-c-yasnippet")

(defgroup helm-c-yasnippet nil
  "helm config yasnippet"
  :group 'helm)

(defcustom helm-c-yas-not-display-dups t
  "if non-nil not display duplicate snippet otherwise display all snippet"
  :type 'boolean
  :group 'helm-c-yasnippet)

(defcustom helm-c-yas-display-msg-after-complete t
  "if non-nil display snippet key message in minibuffer after Complete"
  :type 'boolean
  :group 'helm-c-yasnippet)

(defcustom helm-c-yas-snippets-dir-list nil
  "list of directory used to find snippet file"
  :type '(repeat (directory
                  :tag "snippet-directory"))
  :group 'helm-c-yasnippet)

(defcustom helm-c-yas-space-match-any-greedy nil
  "if non-nil helm pattern space match anyword greedy.
pattern regexp: \"if else\" replace to \"if.*else\"
match \"if (...) { ... } else { ... }\" and \"if, elsif, else ...\"
quite convenience
Default: nil"
  :type 'boolean
  :group 'helm-c-yasnippet)

(defcustom helm-c-yas-display-key-on-candidate nil
  "if non-nil helm display candidate(snippet name) include key
ex. [for] for (...) { ... }
otherwise display just name
ex. for (...) { ... }"
  :type 'boolean
  :group 'helm-c-yasnippet)

(defvar helm-c-yas-snippets-dir-list nil)
(defadvice yas-load-directory-1 (around helm-yas-build-alist activate)
  (let ((directory (ad-get-arg 0)))
    (when (stringp directory)
      (add-to-list 'helm-c-yas-snippets-dir-list directory)))
  ad-do-it)


(defun helm-c-yas-create-new-snippet (selected-text &optional snippet-file)
  "Create snippet from SELECTED-TEXT into SNIPPET-FILE.
If SNIPPET-FILE is nil, asks file name.
If SNIPPET-FILE does not contain directory, it is placed in default snippet directory."
  (let ((snippet-dir (helm-c-yas-find-recursively
                      (regexp-quote (symbol-name helm-c-yas-cur-major-mode))
                      (expand-file-name
                       (or (car-safe yas/root-directory) yas/root-directory))
                      'snippet-file)))
    (setq snippet-file
          (helm-aif snippet-file
              (expand-file-name snippet-file snippet-dir)
            (read-file-name "create snippet : " snippet-dir snippet-dir)))
    (when (file-exists-p snippet-file)
      (error "can't create file [%s] already exists" (file-name-nondirectory snippet-file)))
    ;; create buffer, insert template
    (find-file snippet-file)
    (insert "# -*- mode: snippet -*-\n#name : \n#key : \n#contributor : myuhe\n# --\n" selected-text)))

(defun helm-c-yas-find-recursively (regexp &optional directory predicate)
  (let ((directory (or directory default-directory))
        (predfunc (case predicate
                    (dir 'file-directory-p)
                    (file 'file-regular-p)
                    (otherwise 'identity)))
        (files (remove-if (lambda (s) (string-match "^\\." (file-name-nondirectory  s))) (directory-files directory t)))
        (found nil)
        (result nil))
    (loop for file in files
          unless found
          do (if (and (funcall predfunc file)
                      (string-match regexp file))
                 (progn (setq found t)
                        (return (file-name-as-directory file)))
               (when (file-directory-p file)
                 (setq result (helm-c-yas-find-recursively regexp file predicate))))
          finally (return result))))


(defun helm-c-yas-build-cur-snippets-alist (&optional table)
  (let ((yas-choose-keys-first nil)
        (yas-choose-tables-first nil)
        (yas-buffer-local-condition 'always))
    (let* ((result-alist '((candidates) (transformed) (template-key-alist)(template-file-alist)))
           (cur-tables
            (if table
                (list table)
              (yas--get-snippet-tables)))
           (hash-value-alist nil))
      (let ((hashes (loop for table in cur-tables
                          collect (yas--table-hash table))))
        (loop for hash in hashes
              do (maphash (lambda (k v)
                            (let (a)
                              (maphash (lambda (n te)
                                         (setq a (append (list (cons k te)) a)))
                                       v)
                              (setq hash-value-alist (append a hash-value-alist))))
                          hash))
        (loop with transformed
              with templates
              with template-key-alist
;;              with template-file-alist			  
              for lst in hash-value-alist
              for key = (car lst)
              for template-struct = (cdr lst)
              for name = (yas--template-name template-struct) ;`yas--template-name'
              for template = (yas--template-content template-struct) ;`yas--template-content'
;;              for file = (yas--template-file template-struct) ;`yas--template-content'			  
              do (progn (push template templates)
                        (push `(,name . ,template) transformed)
						(push `(,template . ,key) template-key-alist)
               ;;         (push `(,template . ,file) template-file-alist)
						)
              finally (progn (push `(candidates . ,templates) result-alist)
                             (push `(transformed . ,transformed) result-alist)
                 ;;            (push `(template-file-alist . ,template-file-alist) result-alist)
                             (push `(template-key-alist . ,template-key-alist) result-alist)))
        result-alist)
      )))

(defun helm-c-yas-get-modes ()
  (let ((cur-major-mode helm-c-yas-cur-major-mode))
    (list cur-major-mode)))

(defun helm-c-yas-get-cmp-context ()
  "Return list (initial-input point-start point-end)
like `yas--current-key'"
  (let ((start (point))
        (end (point))
        (syntax "w_"))
    (condition-case nil
        (save-excursion
          (when mark-active
            (error ""))
          (skip-syntax-backward syntax)
          (setq start (point))
          (values (buffer-substring-no-properties start end) start end))
      (error (values "" (point) (point))))))

(defun helm-c-yas-get-key-by-template (template alist) ;str template
  "Return key"
  (assoc-default template (assoc-default 'template-key-alist alist)))

(defun helm-c-yas-get-candidates (alist)
  "Return list of template"
  (assoc-default 'candidates alist 'eq))

(defun helm-c-yas-get-transformed-list (alist initial-input)
  "Return list of dotlist, (DISPLAY . REAL) DISPLAY is name of snippet, REAL is template of snippet"
  (let ((transformed-list (assoc-default 'transformed alist 'eq)))
    (cond
     ;; display key on candidate ex: [for] for (...) { ... }
     (helm-c-yas-display-key-on-candidate
      (setq transformed-list (remove-if-not (lambda (lst)
                                              (string-match (concat "^" (regexp-quote initial-input)) (car lst)))
                                            transformed-list))
      (setq transformed-list (loop for dotlst in transformed-list
                                   for name = (car dotlst)
                                   for template = (cdr dotlst)
                                   for key = (helm-c-yas-get-key-by-template template alist)
                                   for name-inc-key = (concat "["
                                                              (if (vectorp key) (key-description key) key)
                                                              "] " name)
                                   collect `(,name-inc-key . ,template))))
     ;; default ex: for (...) { ... }
     (t
      (setq transformed-list (remove-if-not (lambda (lst)
                                              (string-match (concat "^" (regexp-quote initial-input)) (car lst)))
                                            transformed-list))))
    (when helm-c-yas-not-display-dups
      (setq transformed-list (delete-dups transformed-list)))
    ;; sort
    (setq transformed-list (sort* transformed-list 'string< :key 'car))
    transformed-list))
 
(defun helm-c-yas-find-snippet-file-by-key (key)
  (let ((modes (helm-c-yas-get-modes))
        (snippet-dirs helm-c-yas-snippets-dir-list))
    (let ((found-path (loop for mode in modes
                            for test-re = (concat (symbol-name mode) "/" key "$")
                            for path =  (helm-c-yas-find-snippet-file-aux test-re snippet-dirs)
                            when path return path)))
      ;; if not found in major-mode try to find in all dirs
      (unless found-path
        (setq found-path (helm-c-yas-find-snippet-file-aux (concat "/" key "$") snippet-dirs)))
      found-path)))

(defun helm-c-yas-find-snippet-file-aux (test-re dirs)
  (loop with done
        with path
        for directory in dirs
        for files = (directory-files directory t)
        unless done
        do (loop for file in files
                 when (string-match test-re file)
                 return (setq done t
                              path file))
        finally return path))

(defun helm-c-yas-find-file-snippet-by-template (template &optional other-window)
 (let* ((path (helm-c-yas-get-path-by-template template))
;;  (let* ((path (assoc-default template (assoc-default 'template-file-alist helm-c-yas-cur-snippets-alist)))
         (ff-func (if other-window 'find-file-other-window 'find-file)))
    (if path
        (funcall ff-func path)
      (message "not found snippet file"))))


(defun helm-c-yas-get-path-by-template (template)
  (let* ((key (helm-c-yas-get-key-by-template template helm-c-yas-cur-snippets-alist))
         (path (helm-c-yas-find-snippet-file-by-key key)))
    path))

(defun helm-c-yas-match (candidate)
  "if customize variable `helm-c-yas-space-match-any-greedy' is non-nil
space match anyword greedy"
  (cond
   (helm-c-yas-space-match-any-greedy
    (let ((re (replace-regexp-in-string "[ \t]+" ".*" helm-pattern)))
      (string-match re candidate)))
   (t
    (string-match helm-pattern candidate))))

(defvar helm-c-yas-cur-snippets-alist nil)
(defvar helm-c-yas-initial-input "")
(defvar helm-c-yas-point-start nil)
(defvar helm-c-yas-point-end nil)
(defvar helm-c-yas-cur-major-mode nil)
(defvar helm-c-yas-selected-text "" "region text if mark-active otherwise \"\"") 
(defvar helm-c-source-yasnippet
  `((name . "Yasnippet")
    (init . (lambda ()
              (setq helm-c-yas-cur-major-mode major-mode)
              (setq helm-c-yas-selected-text (if mark-active (buffer-substring-no-properties (region-beginning) (region-end)) ""))
              (multiple-value-setq
                  (helm-c-yas-initial-input helm-c-yas-point-start helm-c-yas-point-end) (helm-c-yas-get-cmp-context)) ;return values(str point point)
              (setq helm-c-yas-cur-snippets-alist (helm-c-yas-build-cur-snippets-alist))))
    (candidates . (helm-c-yas-get-candidates helm-c-yas-cur-snippets-alist))
    (candidate-transformer . (lambda (candidates)
                               (helm-c-yas-get-transformed-list helm-c-yas-cur-snippets-alist helm-c-yas-initial-input)))
    (action . (("Insert snippet" . (lambda (template)
                                     (yas-expand-snippet template helm-c-yas-point-start helm-c-yas-point-end)
                                     (when helm-c-yas-display-msg-after-complete
                                       (message "this snippet is bound to [ %s ]"
                                                (helm-c-yas-get-key-by-template template helm-c-yas-cur-snippets-alist)))))
               ("Open snippet file" . (lambda (template)
										(helm-c-yas-find-file-snippet-by-template template)))
               ("Open snippet file other window" . (lambda (template)
                                                     (helm-c-yas-find-file-snippet-by-template template t)))
               ("Create new snippet on region" . (lambda (template)
                                                   (helm-c-yas-create-new-snippet helm-c-yas-selected-text)))
               ("Reload All Snippts" . (lambda (template)
                                         (yas-reload-all)
                                         (message "Reload All Snippts done")))
               ("Rename snippet file" . (lambda (template)
                                       (let* ((path (or (helm-c-yas-get-path-by-template template) ""))
                                              (dir (file-name-directory path))
                                              (filename (file-name-nondirectory path))
                                              (rename-to (read-string (concat "rename [" filename "] to: "))))
                                         (rename-file path (concat dir rename-to))
                                         (yas-reload-all))))
               ("Delete snippet file" . (lambda (template)
                                          (let ((path (or (helm-c-yas-get-path-by-template template) "")))
                                            (when (y-or-n-p "really delete?")
                                              (delete-file path)
                                              (yas-reload-all)))))))
    (persistent-action . (lambda (template)
                           (helm-c-yas-find-file-snippet-by-template template)))
    (match . (helm-c-yas-match))))


;;; visit template
(defun helm-c-yas-all-templates ()
  (let ((tables (yas--get-snippet-tables)))
    (loop for table in tables
          append (yas--table-templates table))))

(defun helm-c-yas-flatten-templates (templates)
  (loop for lot in templates ;lot is list of templates
        append lot))

(defun helm-c-yas-snippet-files-candidates ()
  "called in `helm-c-source-yasnippet-snippet-files' candidates"
  (let ((yas-choose-keys-first nil)
        (yas-choose-tables-first nil)
        (yas-buffer-local-condition 'always))
    (with-current-buffer helm-current-buffer
      (mapcar* 'yas-template-file
               (mapcar 'cdr
                        (helm-c-yas-all-templates))))))

;; (helm 'helm-c-source-yasnippet-snippet-files)
(defvar helm-c-source-yasnippet-snippet-files
  '((name . "yasnippet snippet files")
    (candidates . helm-c-yas-snippet-files-candidates)
    (type . file)
    ))


;;; Commands
(defun helm-c-yas-complete ()
  "List of yasnippet snippets using `helm' interface."
  (interactive)
  (helm 'helm-c-source-yasnippet))

(defun helm-c-yas-visit-snippet-file ()
  "List of yasnippet snippet files"
  (interactive)
  (helm 'helm-c-source-yasnippet-snippet-files))

(defun helm-c-yas-create-snippet-on-region (&optional start end file-name)
  "Create a snippet from region."
  (interactive "r")
  (let ((str (buffer-substring-no-properties start end)))
    (helm-c-yas-create-new-snippet str file-name)))
;; (helm-c-yas-create-snippet-on-region (region-beginning) (region-end) "aaaa")

(provide 'helm-c-yasnippet)
;;; helm-c-yasnippet.el ends here
