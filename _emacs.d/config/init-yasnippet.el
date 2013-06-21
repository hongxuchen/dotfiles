(setq yas-snippet-dirs '("~/.emacs.d/snippets")) ;; should be set ahead
(require 'yasnippet)
(yas-global-mode 1)
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                              yas-ido-prompt
                              yas-completing-prompt))
;; use yas-completing-prompt ONLY when `M-x yas-insert-snippet'
(defadvice yas-insert-snippet (around use-completing-prompt activate)
     "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
       (let ((yas-prompt-functions '(yas-completing-prompt)))
             ad-do-it))

(provide 'init-yasnippet)
