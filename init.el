;; FIXME: prevent duplicate load-path nonsense on Harris SC setup, what with using site-lisp and all
;; TODO: change paren mode highlight faces
;; see face-user-default-spec in faces.el TODO

(defconst jkl/env-is-mswin
  (eq system-type 'windows-nt))

(defconst jkl/env-harris-sc
  (if (string= system-name "SD011845") system-name)
  "Harris Standard client name, or nil if not known to be a personal Harris SC environment.")

(defvar jkl/pkg-path "~/emacs.d/packages/")
(defvar jkl/my-dir "~/git/jkl-emacs/")

;; FIXME - move all but my-dir to host specific tree
(when jkl/env-harris-sc
  (setq jkl/pkg-path "C:/prp/emacs-23.2/site-lisp/"))

(add-to-list 'load-path (concat jkl/my-dir "jkl-lib"))

(let ((load-path (cons (file-name-as-directory (concat jkl/my-dir "jkl-cust")) load-path)))
  (load "00-common"))

