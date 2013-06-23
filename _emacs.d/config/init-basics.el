(provide 'init-basics)

(setq user-full-name "Hongxu Chen"
      user-mail-address "leftcopy.chx@gmail.com")

;; jabber
(setq fsm-debug nil
      jabber-debug-log-xml nil
      jabber-debug-keep-process-buffers nil
      jabber-vcard-avatars-retrieve nil
      jabber-chat-buffer-show-avatar nil)
(setq jabber-account-list '(
                            ("leftcopy.chx@gmail.com"
                             (:network-server . "talk.google.com")
                             ;; (:port . 5222)
                             (:connection-type . ssl))))

(setq browse-url-generic-program
      (cond
       (*is-a-mac* "open")
       (*linux* (executable-find "x-www-browser"))))

(require 'google-this)
(google-this-mode t)

;; douban-music
(setq douban-music-default-channel 10)

;; shell settings
(defalias 'shell 'eshell "farewell, my shell!")
(eval-after-load 'exec-path-from-shell
  '(progn
     (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "PATH"))
       (add-to-list 'exec-path-from-shell-variables var))))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(setq explicit-shell-file-name "/usr/bin/zsh"
      shell-command-switch "-ic")
