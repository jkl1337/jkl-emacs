
(jkl/custom-set 'semantic-new-buffer-setup-functions
                (append '((cpp-mode . semantic-default-c-setup)
                          (c-mode . semantic-default-c-setup))
                        semantic-new-buffer-setup-functions))

(global-ede-mode t)
(semantic-load-enable-excessive-code-helpers)
;;(require 'semantic-gcc)

(require 'semanticdb)
(global-semanticdb-minor-mode 1)
(global-semantic-tag-folding-mode 1)

(require 'eassist)
;; (add-hook 'ecb-before-activate-hook
;;        (lambda () (semantic-load-enable-code-helpers)))
(require 'semanticdb-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(require 'semanticdb-ectag)
(semantic-load-enable-primary-exuberent-ctags-support)

;;(global-semantic-idle-tag-highlight-mode 1)
