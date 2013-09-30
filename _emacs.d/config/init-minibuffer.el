(setq ido-ubiquitous-enable-old-style-default nil)
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
      ido-use-faces t
      ;; allow same buffer opened in different frames
      ido-default-buffer-method 'selected-window)
(setq ido-ignore-directories (append ido-ignore-directories '("^auto/$" "\\.prv/" "_region_")))
(setq ido-ignore-files (append ido-ignore-files '("^auto/$" "_region_")))

;; (flx-ido-mode 1)
;; (require 'flx-ido)
;; (setq flx-ido-use-faces nil)

(smex-initialize)

(setq imenu-use-popup-ml)
(setq imenu-eager-completion-buffer nil)
(setq imenu-auto-rescan t)

(setq echo-keystrokes 0)

(provide 'init-minibuffer)
