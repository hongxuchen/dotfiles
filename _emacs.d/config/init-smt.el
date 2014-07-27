(provide 'init-smt)

(add-to-list 'auto-mode-alist '("\\.smt2$" . smt-mode))

;; SMTLIB commands as keywords
(setq
 smtlib-keywords
 '("set-logic"
   "set-option"
   "set-info"
   "declare-sort"
   "define-sort"
   "declare-fun"
   "define-fun"
   "push"
   "pop"
   "assert"
   "check-sat"
   "get-assertions"
   "get-proof"
   "get-unsat-core"
   "get-value"
   "get-assignment"
   "get-option"
   "get-info"
   "exit"))

(setq smtlib-keywords-regexp (regexp-opt smtlib-keywords 'words))

(setq smtlib-keywords nil)


(defun smt-mode-variables ()
  (set (make-local-variable 'comment-start) ";; ")
  (set (make-local-variable 'comment-end) "")
  (setq smtlib-keywords-regexp nil)
  (set (make-local-variable 'font-lock-defaults) smtlib-keywords)
  )

(define-derived-mode smt-mode prog-mode "SMT"
  "Major mode for editing SMT scripts"
  (smt-mode-variables)
  (paredit-mode 1)
  )
