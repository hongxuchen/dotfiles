(setq user-full-name "Hongxu Chen"
      user-mail-address "hongxuchen1989@gmail.com")

;; prompt related
;; used for emacs daemon
(add-hook 'server-visit-hook '(lambda ()
                                (remove-hook
                                 'kill-buffer-query-functions
                                 'server-kill-buffer-query-function)))
(fset 'yes-or-no-p 'y-or-n-p)
(setq suggest-key-bindings t
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

;; TODO
(setq fortune-dir "/usr/share/games/fortunes")
(setq fortune-file (expand-file-name "fortunes" fortune-dir))

(setq browse-url-generic-program
      (cond
       (*is-a-mac* "open")
       (*linux*
        (executable-find "x-www-browser")
        )))

;; google
(require 'google-this)
(google-this-mode 1)
(setq google-translate-default-source-language "en"
      google-translate-default-target-language  "zh-CN")
(autoload 'google-translate-at-point "google-translate" "google translate at point" t)
(autoload 'google-translate-query-translate "google-translate" "google translate" t)

;; simple-dict
(autoload 'dict-lookup-definition "simple-dict" "lookup words through DICT" t)
;; youdao-dict
(autoload 'youdao-dict "youdao-dict" "look up words via youdao dictionary" t)
;; gnus
(autoload 'gnus "init-gnus" "the powerful gnu newsreader" t)
(autoload 'compose-mail "init-gnus" "compose mail using gnus" t)
;; apt-utils
(eval-after-load "apt-utils" '(require 'apt-utils-ido))
;; smex
;; (eval-after-load 'smex '(defun smex-show-key-advice (command) ()))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(provide 'init-misc)
