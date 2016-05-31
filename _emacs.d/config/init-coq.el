(if *is-cocoa-emacs*
    (load "/usr/local/Cellar/proof-general/4.2/share/emacs/site-lisp/proof-general/generic/proof-site"))
;; (add-to-list 'auto-mode-alist '("\\.v$" . coq-mode))
(add-hook 'coq-mode-hook #'company-coq-mode)
'(coq-default-undo-limit 10000)
(setq company-coq-live-on-the-edge t) 
(add-hook 'proof-ready-for-assistant-hook (lambda () (show-paren-mode 0)))
(eval-after-load 'coq-mode '(show-paren-mode 0))
(provide 'init-coq)
