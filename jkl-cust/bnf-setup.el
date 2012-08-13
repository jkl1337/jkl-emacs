
(define-generic-mode 'bnf-emv-mode
  '("//") ;; comment char: inapplicable because # must be at start of line
  nil
  '(
    ("^<.*?>" . 'font-lock-function-name-face) ;; LHS nonterminals
    ("<.*?>" . 'font-lock-builtin-face)
    ("\\<PRONAS\\>" . 'font-lock-builtin-face)
    ("\|" . 'font-lock-warning-face) ;; "OR" symbol
   )
  '("\\.bnf\\'") ;; filename suffixes
  '((lambda ()
      (push (concat
             (regexp-opt '("!grammar" "!language" "!start" "!export" "!import" "!slot"
                           "!activatable" "!pronounce" "!optional" "!id" "!repeat" "!tag")
                         t) "\\_>") generic-font-lock-keywords)
         ))
  "Major mode for Nuance BNF highlighting.")