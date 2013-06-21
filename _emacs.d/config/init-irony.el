(provide 'init-irony)
;; for self-contained use
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/irony/irony-mode/elisp"))
(require 'auto-complete)
(require 'yasnippet)
(require 'irony)

(setq ac-disable-faces (delq 'font-lock-string-face ac-disable-faces))
;; the ac plugin will be activated in each buffer using irony-mode
(irony-enable 'ac)             ; hit C-RET to trigger completion
;; (irony-enable 'irony-flycheck)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
