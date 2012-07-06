
(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)
;; (require 'emms-setup)

;; (when (and nil jkl/mswinp)
;;   (setq pylookup-program (concat pylookup-dir "pylookup.bat"))
;;   (unless (file-exists-p pylookup-program)
;;     (let ((explicit-shell-file-name "cmd.exe"))
;;       (with-temp-buffer
;;         (write-region
;;          (concat "@python "
;;                  (convert-standard-filename
;;                   (concat pylookup-dir "pylookup.py")) " %*")
;;          nil pylookup-program)))
;;     t))

(setq yos-root-dir "/home/jluebs/p4/w/vhs/yosemite/branches/perth/")

(let
    ((yos-src-alist '((yos-core-src-dir . "app_core/src")
                      (yos-app-src-dir . "app_server/src")
                      (yos-devcore-src-dir . "device_core/src")
                      (yos-eventpub-src-dir . "event_publisher/src")
                      (yos-icore-src-dir . "integration_core/src")
                      (yos-iengine-src-dir . "integration_engine/src")
                      (yos-wc-src-dir . "management/src")
                      (yos-web-src-dir . "web/src")))
     (result))
  
  (dolist (elem yos-src-alist result)
    (setq result (cons
                  (concat yos-root-dir (cdr elem))
                  result))))
