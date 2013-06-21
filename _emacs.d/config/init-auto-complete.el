;; @see http://cx4a.org/software/auto-complete/manual.html
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-expand-on-auto-complete nil)
(setq popup-use-optimized-column-computation nil)
(setq ac-auto-start nil)
(setq ac-dwim t)
(ac-set-trigger-key "TAB") ; after input prefix, press TAB key ASAP
;; Use C-n/C-p to select candidate ONLY when completion menu is displayed
(setq ac-use-menu-map t)
(setq ac-fuzzy-enable t)
(ac-config-default)
(ac-flyspell-workaround)
;; (setq ac-ignore-case 'smart) ;;default

(defun add-ac-trigger-command (command)
  (if (functionp command)
      (setq ac-trigger-commands (cons command ac-trigger-commands))))
(add-ac-trigger-command 'backward-delete-char-untabify)
(add-ac-trigger-command 'autopair-backspace)

(global-set-key (kbd "M-/") 'ac-complete-filename)
(set-default
 'ac-sources
 (append  ac-sources '(ac-source-filename ac-source-files-in-current-dir) ))

;; hippie-expand
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name))
;; try-expand-dabbrev
;; try-expand-dabbrev-all-buffers
;; try-expand-dabbrev-from-kill
(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'init-auto-complete)
