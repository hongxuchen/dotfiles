(defcustom pdf-latex-command "pdflatex" "pdflatex command"
  :group 'preview)

(if (eq window-system 'w32) (setq view-buffer-command "start")
 (if (eq system-type 'gnu/linux) (setq view-buffer-command "open"))) 

(if (eq window-system 'w32) (setq view-buffer-command "start")
  (cond ((eq system-type 'gnu/linux) (setq view-buffer-command "xdg-open"))
        ((eq system-type 'darwin) (setq view-buffer-command "open"))
        (t (setq view-buffer-command "xdg-open"))))

(defun latex-preview-update ()
  (interactive)
  (if (eq (call-process pdf-latex-command nil "*pdflatex-buffer*" nil buffer-file-name) 1)
      (if (y-or-n-p "PDF Generation Failed. View Errors?") (switch-to-buffer "*pdflatex-buffer*"))
    (start-process "Preview"
                   (get-buffer-create "*pdflatex-buffer*")
                   view-buffer-command
                   (replace-regexp-in-string ".tex" ".pdf" buffer-file-name)
                   )))

(defun latex-preview-pane-update ()
  (interactive)
  (when (eq major-mode 'latex-mode)
    (progn
      (message "Updating LaTeX Preview Pane")
      (latex-preview-pane-update-p))))

(defun latex-preview-pane-update-p ()
  (if (eq (call-process pdf-latex-command nil "*pdflatex-buffer*" nil buffer-file-name) 1)
      (if (y-or-n-p "PDF Generation Failed. View Errors?") (switch-to-buffer "*pdflatex-buffer*"))
    (let ((tex-buff (current-buffer))
          (pdf-buff (replace-regexp-in-string ".tex" ".pdf" (buffer-file-name))))

      (if (not (eq (get-buffer-window pdf-buff) nil))
          (progn
            (switch-to-buffer-other-window pdf-buff)
            (switch-to-buffer-other-window tex-buff)
            )
        ))))

(add-hook 'after-save-hook 'latex-preview-pane-update)

(provide 'latex-preview-pane)
