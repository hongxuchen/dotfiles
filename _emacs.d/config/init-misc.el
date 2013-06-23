;; basic settings
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq goto-address-mail-face 'link)
(setq disabled-command-function nil)
(setq-default regex-tool-backend 'perl)
(setq echo-keystrokes 0.1)
;; (setq next-line-add-newlines nil)

(progn
  (setq x-select-enable-clipboard t) ;; Copied
  (setq x-select-enable-primary t) ;; selected
  (setq select-active-regions nil)
  (setq mouse-drag-copy-region t)
  (global-set-key [mouse-2] 'mouse-yank-at-click))

(setq isearch-allow-scroll t)

;; backup files
(setq
 backup-by-coping t ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.backups"))
 delete-old-versions t
 kept-new-versions 3
 kept-old-versions 2
 version-control t  ;use versioned backups
 vc-make-backup-files nil ;; no backup for vc files
 )

(setq list-command-history-max 64)

;; display time
(setq display-time-24hr-format nil
      display-time-day-and-date t)

;; fill issues
(setq-default fill-column 80
              auto-fill-mode 1)



(add-hook 'find-file-hook 'goto-address-prog-mode) ;; buttonize URLs
(add-hook 'find-file-hook
          (lambda ()
            (when buffer-read-only
              (view-buffer (current-buffer) 'kill-buffer-if-not-modified))))

;; vc issues
(setq vc-follow-symlinks t
      vc-stay-local t)

;; for search words using dictionary-el
(setq dictionary-server "localhost")

;; google-translate
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language  "zh-CN")

;; prompt related
;; used for emacs daemon
(add-hook 'server-visit-hook '(lambda ()
                                (remove-hook
                                 'kill-buffer-query-functions
                                 'server-kill-buffer-query-function
                                 )))
(setq suggest-key-bindings nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
;; TODO local variable without query unsafe
(setq enable-local-variables :all)
(setq enable-local-eval t)
(setq dired-enable-local-variables :all)
(setq revert-without-query t)

;; (add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'") ;;only with crontab-mode plugin
(add-auto-mode 'tcl-mode "Portfile\\'")

(setq source-directory (expand-file-name "~/.bin/builds/emacs"))

;; TODO
(setq fortune-dir "/usr/share/games/fortunes")
(setq fortune-file (expand-file-name "fortunes" fortune-dir))

(provide 'init-misc)
