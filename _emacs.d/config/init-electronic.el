;; ------------------------------------------------------------------------------
;; autoinsert
;; ------------------------------------------------------------------------------
(require 'autoinsert)
(autoload 'file-template-find-file-not-found-hook "file-template" nil t)
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
(global-auto-complete-mode 1)
(setq ac-expand-on-auto-complete t
      popup-use-optimized-column-computation nil
      ac-auto-start 2
      ac-dwim t
      ac-auto-show-menu t
      ac-use-fuzzy nil
      ac-use-comphist nil
      ac-comphist-threshold 0.5
      ac-use-quick-help nil
      ac-quick-help-delay 0.1
      ac-ignore-case nil
      ac-quick-help-prefer-pos-tip nil)
(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
(define-key ac-mode-map (kbd "C-c H") 'ac-last-help)
(add-to-list 'ac-dictionary-directories '"~/.emacs.d/.dict")

;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
(ac-set-trigger-key "TAB") ; after input prefix, press TAB key ASAP
;; Use C-n/C-p to select candidate ONLY when completion menu is displayed
(setq ac-use-menu-map t)
;; (setq ac-fuzzy-enable nil)
(ac-config-default)
(ac-flyspell-workaround)
;; (setq ac-ignore-case 'smart) ;;default

(dolist (command '(
                   backward-delete-char-untabify
                   autopair-backspace
                   ))
  (add-to-list 'ac-trigger-commands command))

(dolist (mode '(cmake-mode
                latex-mode
                makefile-gmake-mode
                makefile-automake-mode
                ))
  (add-to-list 'ac-modes mode))

(set-default 'ac-source
             '(ac-source-filename
               ac-source-files-in-current-dir
               ac-source-features
               ac-source-functions
               ac-source-yasnippet
               ac-source-variables
               ac-source-symbols
               ac-source-features
               ac-source-functions
               ac-source-yasnippet
               ac-source-variables
               ac-source-symbols
               ac-source-abbrev
               ac-source-dictionary
               ac-source-words-in-same-mode-buffers
               ))

(defun my-tex-mode-ac-setup ()
  (require 'ac-math)
  (add-to-list 'ac-modes 'latex-mode)
  (setq ac-math-unicode-in-math-p t)
  (setq ac-sources
        (append
         '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands) ac-sources))
  )

(defun my-elisp-mode-ac-setup ()
  (ac-emacs-lisp-mode-setup)
  )

(defun my-cc-mode-ac-setup ()
  (interactive)
  (make-local-variable 'ac-auto-start)
  (setq ac-auto-start 3)
  (require 'irony)
  (irony-mode 1)
  (irony-ac-enable)
  ;; (require 'company-rtags)
  ;; (require 'rtags-ac)
  ;; (setq ac-sources '(ac-source-rtags))
  (setq ac-sources
        '(
          ac-source-irony
          ac-source-words-in-same-mode-buffers
          ac-source-dictionary
          ac-source-yasnippet
          ))
  )

;; ------------------------------------------------------------------------------
;; company-mode
;; ------------------------------------------------------------------------------
;; (global-company-mode 1)
;; (setq company-idle-delay 0)
;; (setq company-dabbrev-other-buffers t)
;; (setq company-dabbrev-downcase nil)
;; (setq company-dabbrev-minimum-length 2)
;; (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
;; (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

;; ------------------------------------------------------------------------------
;; hippie-expand
;; ------------------------------------------------------------------------------
(setq dabbrev-case-fold-search nil)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name))

(provide 'init-electronic)
