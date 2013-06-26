(require 'ido-ubiquitous)
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point nil
      ido-ubiquitous-enable-compatibility nil
      ido-auto-merge-work-directories-length -1
      ido-enable-regexp nil
      ido-max-prospects 12
      ido-max-window-height 2
      ido-use-virtual-buffers t
      ido-create-new-buffer 'always
      ido-save-directory-list-file "~/.emacs.d/.ido.last"
      ;; allow same buffer opened in different frames
      ido-default-buffer-method 'selected-window)

(setq ido-ignore-directories (append ido-ignore-directories '("^auto/$" "\\.prv/" "_region_")))
(setq ido-ignore-files (append ido-ignore-files '("^auto/$" "_region_")))

(smex-initialize)

(setq imenu-use-popup-ml)
(setq imenu-eager-completion-buffer nil)
(setq imenu-auto-rescan t)

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(provide 'init-minibuffer)
