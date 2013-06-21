(provide 'init-shell)

;; TODO switch to xterm/tmux smoothly, especially CWD

(eval-after-load 'exec-path-from-shell
  '(progn
     (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
       (add-to-list 'exec-path-from-shell-variables var))))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; (defun track-shell-directory/procfs ()
;;     (shell-dirtrack-mode 0)
;;     (add-hook 'comint-preoutput-filter-functions
;;               (lambda (str)
;;                 (prog1 str
;;                   (when (string-match comint-prompt-regexp str)
;;                     (cd (file-symlink-p
;;                          (format "/proc/%s/cwd" (process-id
;;                                                  (get-buffer-process
;;                                                   (current-buffer)))))))))
;;               nil t))

;; ;; prevent the command line to stay at the bottom of the window
;; (add-hook 'shell-mode-hook
;;           (lambda()
;;             (track-shell-directory/procfs)
;;             (remove-hook 'comint-output-filter-functions
;;                          'comint-postoutput-scroll-to-bottom t)))

(add-hook 'sh-set-shell-hook 'flymake-shell-load)
(setq explicit-shell-file-name "/bin/bash"
      shell-command-switch "-ic")

;; (autoload 'bash-completion-dynamic-complete
;;   "bash-completion"
;;   "BASH completion hook")
;; (add-hook 'shell-dynamic-complete-functions
;;   'bash-completion-dynamic-complete)
;; (add-hook 'shell-command-complete-functions
;;   'bash-completion-dynamic-complete)
;; (defun sh-send-line-or-region (&optional step)
;;   (interactive ())
;;   (let ((proc (get-process "shell"))
;;         pbuf min max command)
;;     (unless proc
;;       (let ((currbuff (current-buffer)))
;;         (shell)
;;         (switch-to-buffer currbuff)
;;         (setq proc (get-process "shell"))
;;         ))
;;     (setq pbuff (process-buffer proc))
;;     (if (use-region-p)
;;         (setq min (region-beginning)
;;               max (region-end))
;;       (setq min (point-at-bol)
;;             max (point-at-eol)))
;;     (setq command (concat (buffer-substring min max) "\n"))
;;     (with-current-buffer pbuff
;;       (goto-char (process-mark proc))
;;       (insert command)
;;       (move-marker (process-mark proc) (point))
;;       ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
;;     (process-send-string  proc command)
;;     (display-buffer (process-buffer proc) t)
;;     (when step
;;       (goto-char max)
;;       (next-line))
;;     ))

;; (defun sh-send-line-or-region-and-step ()
;;   (interactive)
;;   (sh-send-line-or-region t))
;; (defun sh-switch-to-process-buffer ()
;;   (interactive)
;;   (pop-to-buffer (process-buffer (get-process "shell")) t))

(eval-after-load 'esh-opt '(progn (require 'init-eshell)))
