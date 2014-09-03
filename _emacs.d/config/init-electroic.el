;; ------------------------------------------------------------------------------
;; autoinsert
;; ------------------------------------------------------------------------------
(require 'autoinsert)
(setq auto-insert-query nil)
(setq file-template-insert-automatically t)
(add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)
(add-hook 'find-file-hooks 'auto-insert)
(define-auto-insert 'sh-mode '(nil "#!/bin/bash\n\n"))
(define-auto-insert 'python-mode '(nil "#!/usr/bin/python3\n\n"))

;; ------------------------------------------------------------------------------
;; yasnippet
;; ------------------------------------------------------------------------------

(setq yas-snippet-dirs '("~/.emacs.d/snippets")) ;; should be set ahead
(setq auto-mode-alist (cons '("\\.yas$"
                              . snippet-mode) auto-mode-alist))
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-verbosity 0)
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))

(defadvice yas-insert-snippet (around use-completing-prompt activate)
  "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
  (let ((yas-prompt-functions '(yas-completing-prompt)))
    ad-do-it))

;; ------------------------------------------------------------------------------
;; auto-complete
;; ------------------------------------------------------------------------------
;; @see http://cx4a.org/software/auto-complete/manual.html
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-expand-on-auto-complete t)
(setq popup-use-optimized-column-computation nil)
(setq ac-auto-start 2)
(setq ac-dwim t)
(setq ac-auto-show-menu t)
(setq ac-use-fuzzy nil)
(setq ac-use-comphist nil)
(setq ac-comphist-threshold 0.5)
(setq ac-use-quick-help nil)
(setq ac-quick-help-delay 0.1)
(setq ac-ignore-case nil)
(setq ac-quick-help-prefer-pos-tip nil)
(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
(define-key ac-mode-map (kbd "C-c H") 'ac-last-help)
(add-to-list 'ac-dictionary-directories '"~/.emacs.d/.dict")

(setq ac-modes (append ac-modes '(makefile-gmake-mode makefile-automake-mode)))

;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
(ac-set-trigger-key "TAB") ; after input prefix, press TAB key ASAP
;; Use C-n/C-p to select candidate ONLY when completion menu is displayed
(setq ac-use-menu-map t)
;; (setq ac-fuzzy-enable nil)
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
 (append  '(ac-source-filename ac-source-files-in-current-dir) ac-sources))

(setq dabbrev-case-fold-search nil)
;; hippie-expand
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name))
(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'init-electroic)
