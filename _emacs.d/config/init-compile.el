(provide 'init-compile)

(setq-default gud-chdir-before-run nil)
(defun hack-gud-mode ()
  (when (string= major-mode "gud-mode")
    (goto-char (point-max))))

(defadvice switch-to-buffer (after switch-to-buffer-after activate)
  (hack-gud-mode))

;; windmove-do-window-select is from windmove.el
(defadvice windmove-do-window-select (after windmove-do-window-select-after activate)
  (hack-gud-mode))

;; show current line of code
(defadvice gdb-display-source-buffer
  (after ad-hl-line-source-buffer (buffer) activate)
  (with-current-buffer buffer (hl-line-mode 1)))

;; smarter-compile
(require 'smarter-compile)
(add-to-list
 'smart-compile-alist
 '("\\.css\\'"   .   "/bin/csslint.js --format=compiler %f"))
(autoload 'smarter-compile "smarter-compile" "smarter compile current file")

(setq compile-command "make "
      compile-history (list "make" "make clean"))
(setq compilation-read-command nil)
(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match-p "exited abnormally" str)
            (message "contains errors, press C-x ` to visit")
          (with-current-buffer buf
            (goto-char (point-min))
            (unless (search-forward "warning:" nil t)
              (winner-undo))))))
