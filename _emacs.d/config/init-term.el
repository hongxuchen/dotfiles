(defvar my-terminal-process)
(defconst my-terminal-name "*terminal*")
(autoload 'term-send-string "term" nil t)
(defun terminal (target-directory)
  "Switch to terminal. Launch if nonexistant."
  (if (get-buffer my-terminal-name)
      (progn
        (switch-to-buffer my-terminal-name)
        (term-send-string my-terminal-process (format "cd %s\n" target-directory))
        (goto-char (point-max)))
    (progn
      (term "/bin/zsh"))
    (setq my-terminal-process (get-buffer-process my-terminal-name))))

(global-set-key (kbd "M-<f2>") (lambda () (interactive)
                                 (terminal default-directory)))

(setq term-scroll-to-bottom-on-output t)

(setq term-mode-hook nil)
(add-hook 'term-mode-hook
          (lambda ()
            (term-set-escape-char ?\C-x)
            (define-key term-raw-map (kbd "<f4>") 'bury-buffer)
            (yas-minor-mode -1)
            (autopair-mode -1)))

(provide 'init-term)
