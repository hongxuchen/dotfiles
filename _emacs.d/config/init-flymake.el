(setq flymake-gui-warnings-enabled nil)

(eval-after-load 'flymake
  '(progn
     (setq flymake-allowed-file-name-masks
           (cons '("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\|h\\|hpp\\)\\'"
                   flymake-clang-init
                   flymake-simple-cleanup
                   flymake-get-real-file-name)
                 flymake-allowed-file-name-masks))
     (global-set-key (kbd "M-`") 'flymake-goto-next-error)
     (setq flymake-no-changes-timeout 2)
     (defun flymake-can-syntax-check-file (file-name)
       "Determine whether we can syntax check FILE-NAME.
Return nil if we cannot, non-nil if we can."
       (if (and file-name (flymake-get-init-function file-name)) t nil))
     ))

(defun flymake-clang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory (buffer-file-name)))))
    (list "clang"
          (append irony-compile-flags (list "-fsyntax-only" local-file)))))

(eval-after-load 'flymake
  '(progn
     (push '("\\.hpp\\'" flymake-simple-make-init) flymake-allowed-file-name-masks)
     (push '("\\.hh\\'" flymake-simple-make-init) flymake-allowed-file-name-masks)))

;; (require 'flycheck)
;; (global-flycheck-mode 1)
(provide 'init-flymake)
