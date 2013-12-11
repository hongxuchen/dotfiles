(global-linum-mode t)

;; (if (window-system)
;;     (setq linum-format 'dynamic)
;;   (setq linum-format "%3d "))
(setq linum-format "%3d ")

(setq linum-mode-inhibit-modes-list '(eshell-mode
                                      shell-mode
                                      help-mode
                                      speedbar-mode
                                      jabber-roster-mode
                                      Man-mode
                                      woman-mode
                                      jabber-chat-mode
                                      compilation-mode
                                      calc-mode
                                      calc-trail-mode
                                      comint-mode
                                      inf-ruby-mode
                                      gud-mode
                                      term-mode
                                      w3m-mode
                                      gnus-group-mode
                                      gnus-summary-mode
                                      gnus-article-mode
                                      calendar-mode))
(defadvice linum-on (around linum-on-inhibit-for-modes activate)
  "Stop the load of linum-mode for some major modes."
  (unless (member major-mode linum-mode-inhibit-modes-list)
    ad-do-it))

(provide 'init-linum)
