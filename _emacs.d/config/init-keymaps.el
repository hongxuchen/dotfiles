(define-key minibuffer-local-map [escape] 'keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'keyboard-quit)
(global-set-key "\C-x\C-g" 'keyboard-quit)

(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-w") 'backward-kill-word)
;; (global-set-key (kbd "<C-tab>") 'yas-expand)
(global-set-key (kbd "C-c l") 'yas-insert-snippet)

(global-unset-key (kbd "<f1>"))
(global-set-key (kbd "<f1>") 'gnus)
(global-set-key (kbd "<f5>") 'normal-mode)

(global-set-key (kbd "C-h C-l") 'dict-lookup-definition)
(global-set-key (kbd "C-h C-s") 'my-goto-scratch-buffer)
(global-set-key (kbd "<f2>") 'repeat-complex-command)


(global-set-key (kbd "<C-f9>") 'smarter-compile)
(global-set-key (kbd "<M-f2>") 'eshell)

;; (global-set-key "\C-cx" 'clipboard-kill-ring-save)
;; (global-set-key "\C-cc" 'copy-region-as-kill)

(global-set-key "\C-x\C-t" nil) ;;origin is transpose-lines
(global-set-key "\M-t" nil) ;; transpose-words
(global-set-key "\M-u" nil) ;; upcase-word
(global-set-key "\M-l" nil) ;; downcase-word
(global-set-key "\C-x\C-u" nil) ;; upcase-region
(global-set-key "\C-x\C-l" nil) ;; downcase-word

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; helpers
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)

(global-set-key (kbd "C-x C-d") 'ido-dired)
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x d") 'ido-dired)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key "\M-x" 'smex)
(global-set-key "\C-ci" 'idomenu)

(provide 'init-keymaps)
