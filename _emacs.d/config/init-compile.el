(provide 'init-compile)

(setq gdb-many-windows t
      gdb-show-main t
      gud-chdir-before-run nil
      gdb-create-source-file-list nil)

(defun hack-gud-mode ()
  (when (string= major-mode "gud-mode")
    (run-with-timer 0.25 nil (goto-char (point-max)))))
(defadvice switch-to-buffer (after switch-to-buffer-after activate)
  (hack-gud-mode))
(defadvice switch-window (after switch-window-after activate)
  (hack-gud-mode))
(defadvice windmove-do-window-select (after windmove-do-window-select-after activate)
  (hack-gud-mode))
(defadvice yas-expand (after yas-expand-after activate)
  (hack-gud-mode))

(setq gud-mode-hook nil)
(add-hook 'gud-mode-hook (lambda ()
                           ;; (tooltip-mode 1)
                           ;; (gud-tooltip-mode t)
                           (autopair-mode -1)))

(defadvice gdb-display-source-buffer
  (after ad-hl-line-source-buffer (buffer) activate)
  (with-current-buffer buffer (hl-line-mode 1)))

(eval-after-load "gdb-mi"
  `(progn
     (defun gdb-setup-windows ()
       "Simplified gdb windows"
       (gdb-get-buffer-create 'gdb-stack-buffer)
       (set-window-dedicated-p (selected-window) nil)
       (switch-to-buffer gud-comint-buffer)
       (delete-other-windows)
       (let ((win0 (selected-window))
             (win1 (split-window nil nil 'left))
             (win2 (split-window-below (/ (* (window-height) 3) 4)))
             )
         (select-window win2)
         (gdb-set-window-buffer (gdb-stack-buffer-name))
         (select-window win1)
         (set-window-buffer
          win1
          (if gud-last-last-frame
              (gud-find-file (car gud-last-last-frame))
            (if gdb-main-file
                (gud-find-file gdb-main-file)
              (list-buffers-noselect))))
         (setq gdb-source-window (selected-window))
         (let ((win3 (split-window nil (/ (* (window-height) 3) 4))))
           (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win3))
         (select-window win0)
         ))))

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
            (format "save breakpoints ~/emacs.d/.gdb/%s-breakpoints.gdb"
                    gud-process-name))))))

(defun gdb-restore-breakpoints ()
  "Restore the saved breakpoint definitions as a script."
  (interactive)
  (let ((breakpoints-file (format "~/emacs.d/.gdb/%s-breakpoints.gdb"
                                  (gud-get-process-name))))
    (if (file-exists-p breakpoints-file)
        (gud-basic-call (format "source %s" breakpoints-file)))))

(defun gud-kill()
  "Kill gdb-buffer."
  (interactive)
  (gdb-save-breakpoints)
  ;; (with-current-buffer gud-comint-buffer (comint-skip-input))
  ;; (kill-process (get-buffer-process gud-comint-buffer))
  (kill-buffer)
  (delete-window))

(setq gdb-mode-hook nil)
(add-hook 'gdb-mode-hook
          (lambda ()
            (gdb-restore-breakpoints)
            (local-set-key (kbd "C-x k") 'gud-kill)))

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
