(add-hook 'clojure-mode-hook 'paredit-mode)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(add-hook 'nrepl-mode-hook
          (lambda ()
            (subword-mode)
            (ac-nrepl-setup)
            (nrepl-turn-on-eldoc-mode)
            (paredit-mode)))

(setq clojurescript-clj-repl (concat jkl/my-dir "bin/browser-repl"))
