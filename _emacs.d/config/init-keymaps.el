(global-set-key "\C-x\C-g" 'keyboard-quit)

;; (global-set-key (kbd "<tab>") 'company-complete-comon)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key [mouse-2] nil)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-w") 'backward-kill-word)

;; (global-set-key (kbd "<C-tab>") 'yas-expand)
(global-set-key (kbd "C-c l") 'yas-insert-snippet)

(global-set-key (kbd "<f5>") 'normal-mode)

(global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "C-M-f") 'toggle-fullscreen)

(global-set-key (kbd "C-h C-l") 'dict-lookup-definition)
(global-set-key (kbd "C-h C-s") 'my-goto-scratch-buffer)
(global-set-key (kbd "<f2>") 'repeat-complex-command)

(global-set-key (kbd "<C-f9>") 'smarter-compile)
(global-set-key (kbd "<f9>") 'smarter-compile)

(global-set-key (kbd "<f10>") 'my-terminal)
(global-set-key (kbd "M-<f1>")
                (lambda () (interactive)
                  (start-process "XTerm" nil "xterm")))

(global-set-key (kbd "C-c e") 'my-eval-and-replace)
(global-set-key (kbd "C-x j") 'recentf-open-files)

(global-set-key "\C-x\C-z" nil) ;;origin is transpose-lines
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

(global-set-key "\C-x\C-k" 'ido-kill-buffer)
(global-set-key (kbd "C-x d") 'ido-dired)
(global-set-key (kbd "C-x C-d") 'find-name-dired)
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key "\M-x" 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update)
(global-set-key "\C-ci" 'idomenu)

;;; more hijk
(defun find-loadfile-by-map (map)
  "Find load file by MAP."
  (case map
    ('Info-mode-map "info")
    ('ebrowse-tree-mode-map "Tree")
    ))
(dolist (map
         `(Info-mode-map
           ebrowse-tree-mode-map
           ))
  (let ((file (find-loadfile-by-map map)))
    (eval-after-load file
      `(progn
         (define-key ,map "h" 'backward-char)
         (define-key ,map "l" 'forward-char)
         (define-key ,map "j" 'next-line)
         (define-key ,map "k" 'previous-line)))))

(provide 'init-keymaps)
