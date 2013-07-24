(provide 'init-keymaps)

(define-key minibuffer-local-map [escape] 'keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'keyboard-quit)
(global-set-key "\C-x\C-g" 'keyboard-quit)

(global-set-key [mouse-2] 'mouse-yank-at-click)

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
(global-set-key (kbd "<f9>") 'smarter-compile)
(global-set-key (kbd "M-<f1>")
                (lambda () (interactive)
                  (start-process "XTerm" nil "xterm")))
(global-set-key (kbd "<M-f3>") 'eshell)

;; (global-set-key "\C-cx" 'clipboard-kill-ring-save)
;; (global-set-key "\C-cc" 'copy-region-as-kill)

(global-set-key (kbd "C-c e") 'my-eval-and-replace)

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

;;windmove
(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window

(global-set-key (kbd "C-x C-d") 'ido-dired)
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x d") 'ido-dired)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key "\M-x" 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update)
(global-set-key "\C-ci" 'idomenu)

(defun find-loadfile-by-map (map)
  "Find load file by MAP."
  (case map
    ('Info-mode-map "info")
    ('ebrowse-tree-mode-map "Tree")))

(dolist (map `(Info-mode-map ebrowse-tree-mode-map))
  (let ((file (find-loadfile-by-map map)))
    (eval-after-load file
      `(progn
         (define-key ,map "h" 'backward-char)
         (define-key ,map "l" 'forward-char)
         (define-key ,map "j" 'next-line)
         (define-key ,map "k" 'previous-line)))))
