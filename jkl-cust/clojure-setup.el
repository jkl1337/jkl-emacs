(add-hook 'clojure-mode-hook 'paredit-mode)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(add-hook 'nrepl-interaction-mode-hook
          (lambda ()
            (ac-nrepl-setup)
            (nrepl-turn-on-eldoc-mode)
            (paredit-mode)))

(add-hook 'nrepl-mode-hook
          (lambda ()
            (subword-mode)
            (ac-nrepl-setup)
            (paredit-mode)))

