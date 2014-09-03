(add-to-list
 'auto-mode-alist
 (cons (concat "\\." (regexp-opt '("sh" "csh" "tcsh" "bash" "zsh") t) "\\'") 'sh-mode))

(defun my-sh-mode-setup ()
  (setq-default sh-basic-offset 4
                sh-indentation 4
                sh-indent-for-case-label 0
                sh-indent-for-case-alt '+)
  )

(eval-after-load 'sh
  '(my-sh-mode-setup)
  )

(add-hook 'sh-mode-hook 'flymake-shell-load)

(provide 'init-sh)
