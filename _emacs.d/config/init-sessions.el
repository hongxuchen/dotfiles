(setq session-use-package t)
(setq session-save-file (expand-file-name "~/.emacs.d/.session"))
(add-hook 'after-init-hook 'session-initialize)

;; recent files
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"
                        "/home/[a-z]\+/\\."
                        ))
(recentf-mode t)

(provide 'init-sessions)
