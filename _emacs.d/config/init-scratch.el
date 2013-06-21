(setq persistent-scratch-filename "~/.emacs.d/.my-scratch")
(setq persistent-scratch-backup-directory "~/.emacs.d/.my-scratch-backups/")

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
  `persistent-scratch-filename', making a backup copy in
  persistent-scratch-backup-directory."
  (let ((buf (get-buffer "*scratch*")))
    (when buf
      (with-current-buffer buf
        (write-region (point-min) (point-max)
                      persistent-scratch-filename)))))

;; TODO not meant to work like this
(defun load-persistent-scratch ()
  "Load the contents of `persistent-scratch-filename' into the
  scratch buffer, clearing its contents first."
  (if (file-exists-p persistent-scratch-filename)
      (with-current-buffer (get-buffer "*scratch*")
        (erase-buffer)
        (shell-command (format "cat %s" persistent-scratch-filename) (current-buffer)))))

(add-hook 'kill-emacs-hook 'save-persistent-scratch)

(provide 'init-scratch)
