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

(defun my-company-setup ()
  (make-local-variable 'company-c-headers-path-user)
  (setq company-c-headers-path-system
        '("/usr/include"
          "/usr/local/include"
          "/usr/include/c++/4.9"
          "/usr/include/x86_64-linux-gnu/c++/4.9"
          "/home/hongxu/marple-llvm/llvm-obj/lib/clang/3.6.0/include"
          "/usr/include/x86_64-linux-gnu"
          ))
  (global-company-mode 1)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0)
  (setq company-dabbrev-other-buffers t)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-minimum-length 2)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (message "company-mode setup")
  (setq company-backends
        '(
          company-c-headers
          company-elisp
          ;; company-bbdb
          company-nxml
          company-css
          ;; company-eclim
          ;; company-semantic
          ;; company-clang
          ;; company-xcode
          ;; company-ropemacs
          company-cmake
          company-capf (company-dabbrev-code
                        ;; company-gtags
                        ;; company-etags
                        company-keywords)
          ;; company-oddmuse
          company-files
          company-dabbrev
          ;; company-yasnippet
          )))

(my-company-setup)

(defun my-tex-mode-ac-setup ()
  (require 'company-auctex)
  (company-auctex-init))

(defun my-elisp-mode-ac-setup ()
  )

(defun my-cc-mode-ac-setup ()
  (company-mode 1)
  (require 'company-rtags)
  (make-local-variable 'company-frontends)
  (setq company-frontends
        '(
          company-rtags
          company-pseudo-tooltip-unless-just-one-frontend
          company-echo-metadata-frontend
          company-preview-if-just-one-frontend
          )))

;; ------------------------------------------------------------------------------
;; hippie-expand
;; ------------------------------------------------------------------------------
;; (setq dabbrev-case-fold-search nil)
;; (setq hippie-expand-try-functions-list
;;       '(try-complete-file-name-partially
;;         try-complete-file-name))

(provide 'init-electronic)
