(setq yas-snippet-dirs '("~/.emacs.d/snippets")) ;; should be set ahead
(setq auto-mode-alist (cons '("\\.yas$"
                              . snippet-mode) auto-mode-alist))
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-verbosity 0)
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))

(defadvice yas-insert-snippet (around use-completing-prompt activate)
  "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
  (let ((yas-prompt-functions '(yas-completing-prompt)))
    ad-do-it))

(provide 'init-yasnippet)
