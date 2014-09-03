(setq persistent-scratch-filename (expand-file-name ".my-scratch.el" user-emacs-directory))

(defun save-persistent-scratch ()
  "write contents of *scratch* to the file name `persistent-scratch-filename'"
  (let ((buf (get-buffer "*scratch*")))
    (when buf
      (with-current-buffer buf
        (write-region (point-min) (point-max)
                      persistent-scratch-filename)))))

(defun load-persistent-scratch ()
  "Load contents of `persistent-scratch-filename' into the
  scratch buffer, clearing its contents first."
  (if (file-exists-p persistent-scratch-filename)
      (with-current-buffer (get-buffer "*scratch*")
        (erase-buffer)
        (insert-file-contents-literally persistent-scratch-filename))))

(add-hook 'kill-emacs-hook 'save-persistent-scratch)

(provide 'init-scratch)
