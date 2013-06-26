(provide 'init-misc)

;; kill/yank/paste
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      select-active-regions nil
      mouse-drag-copy-region t
      kill-do-not-save-duplicates t
      mouse-yank-at-point t)

;; file content
(setq backup-by-coping t ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.backups"))
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t
      vc-make-backup-files nil)
(setq find-file-suppress-same-file-warnings t)
(setq view-read-only t)
(setq auto-save-default nil)
(require 'saveplace)
(setq-default save-place t
              save-place-file "~/.emacs.d/saved-places")
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t
      revert-without-query t
      auto-revert-verbose nil)
(setq vc-follow-symlinks t
      vc-stay-local t)

;; prompt related
;; used for emacs daemon
(add-hook 'server-visit-hook '(lambda ()
                                (remove-hook
                                 'kill-buffer-query-functions
                                 'server-kill-buffer-query-function)))
(fset 'yes-or-no-p 'y-or-n-p)
(setq suggest-key-bindings nil
      confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
;; TODO local variable without query unsafe
(setq enable-local-variables :all)
(setq enable-local-eval t)

;; BUG when there are both Uppercase and separator
(setq glasses-face 'bold)
(setq glasses-separator "")

(setq source-directory (expand-file-name "~/.bin/builds/emacs"))

;; TODO
(setq fortune-dir "/usr/share/games/fortunes")
(setq fortune-file (expand-file-name "fortunes" fortune-dir))

;; shell settings
(defalias 'shell 'eshell "farewell, my shell!")
(eval-after-load 'exec-path-from-shell
  '(progn
     (exec-path-from-shell-initialize)
     (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "PATH"))
       (add-to-list 'exec-path-from-shell-variables var))))
(setq explicit-shell-file-name "/usr/bin/zsh"
      shell-command-switch "-ic")

(setq browse-url-generic-program
      (cond
       (*is-a-mac* "open")
       (*linux* (executable-find "x-www-browser"))))

;; google
(require 'google-this)
(google-this-mode t)
(setq google-translate-default-source-language "en"
      google-translate-default-target-language  "zh-CN")

;; nicer naming
(require 'uniquify) ;; 24.3 contained
(setq uniquify-buffer-name-style 'forward
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")
