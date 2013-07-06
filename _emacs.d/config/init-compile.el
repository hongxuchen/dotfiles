(provide 'init-compile)

(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gud-chdir-before-run nil)
(setq gdb-create-source-file-list nil)

(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)

(defun hack-gud-mode ()
  (when (string= major-mode "gud-mode")
    (run-with-timer 0.25 nil (goto-char (point-max)))))

(add-hook 'gud-mode-hook (lambda ()
                           (tooltip-mode 1)
                           (gud-tooltip-mode t)
                           (autopair-mode -1)))

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
(setq compilation-finish-function nil)
(setq compilation-finish-functions nil)
;; (setq compilation-finish-functions
;;       (lambda (buf str)
;;         (if (string-match-p "exited abnormally" str)
;;             (message "contains errors, press C-x ` to visit")
;;           (with-current-buffer buf
;;             (goto-char (point-min))
;;             (unless (search-forward "warning:" nil t)
;;               (winner-undo))))))

(defun gud-break-remove ()
  "Set/clear breakpoint."
  (interactive)
  (save-excursion
    (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)
        (gud-remove nil)
      (gud-break nil))))

(defun gdb-or-gud-go ()
  "If gdb isn't running; run gdb, else call gud-go."
  (interactive)
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer)
           (get-buffer-process gud-comint-buffer)
           (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdba)))
      (funcall (lambda () (gud-call (if gdb-active-process "continue" "run") "")))
    (funcall(lambda () (gdb (gud-query-cmdline 'gdba))))))

(defun gud-kill ()
  "Kill gdb process."
  (interactive)
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  (kill-process (get-buffer-process gud-comint-buffer))
  (delete-window))

(setq auto-mode-alist (append '(("\\.gdb$" . gdb-script-mode)) auto-mode-alist))

(defun gud-get-process-name ()
  (let ((process (get-buffer-process gud-comint-buffer)))
    (if (null process)
        nil
      (process-name process))))

(defun gdb-save-breakpoints ()
  "Save current breakpoint definitions as a script."
  (interactive)
  (let ((gud-process-name (gud-get-process-name)))
    (cond (gud-process-name
           (gud-basic-call
            (format "save breakpoints ~/.%s-breakpoints.gdb"
                    gud-process-name))))))

(defun gdb-restore-breakpoints ()
  "Restore the saved breakpoint definitions as a script."
  (interactive)
  (let ((breakpoints-file (format "~/.%s-breakpoints.gdb"
                                  (gud-get-process-name))))
    (if (file-exists-p breakpoints-file)
        (gud-basic-call (format "source %s" breakpoints-file)))))

(defun gdb-kill-buffer ()
  "Kill gdb-buffer."
  (interactive)
  (gdb-save-breakpoints)
  (kill-buffer))

;; (defun gdb-breakpoint-session ()
;;   (gdb-restore-breakpoints)
;;   (local-set-key (kbd "C-x k") 'gdb-kill-buffer))

;; (add-hook 'gdb-mode-hook 'gdb-breakpoint-session)
