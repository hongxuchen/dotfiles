;; Use C-f during file selection to switch to regular find-file
(require 'ido-ubiquitous)
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point nil
      ido-auto-merge-work-directories-length -1
      ido-use-virtual-buffers t
      ido-create-new-buffer 'always
      ido-save-directory-list-file "~/.emacs.d/.ido.last"
      ;; allow same buffer opened in different frames
      ido-default-buffer-method 'selected-window)
(setq ido-ignore-directories (append ido-ignore-directories '("^auto/$" "\\.prv/" "_region_")))
(setq ido-ignore-files (append ido-ignore-files '("^auto/$" "_region_")))
(smex-initialize)
(setq imenu-auto-rescan t)

(provide 'init-minibuffer)
