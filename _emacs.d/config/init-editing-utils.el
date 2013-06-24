(provide 'init-editing-utils)

(setq-default blink-cursor-delay 0
              fill-column 80
              blink-cursor-interval 0.4
              bookmark-default-file "~/.emacs.d/.bookmarks.el"
              buffers-menu-max-size 20
              regex-tool-backend 'perl
              case-fold-search t
              lazy-highlight-cleanup nil
              case-replace nil
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              indent-tabs-mode nil
              line-spacing 0.2
              set-mark-command-repeat-pop t
              truncate-lines nil
              disabled-command-function nil
              echo-keystrokes 0.1
              Info-use-header-line t
              isearch-allow-scroll t
              help-window-select t
              truncate-partial-width-windows nil
              visible-bell nil)

(setq completion-show-help nil)
(setq kill-whole-line t)

(tooltip-mode -1)
(transient-mark-mode t)
(delete-selection-mode t)
(global-pointback-mode t)

;; grep
(setq grep-program "grep"
      grep-command "grep -inH"
      grep-highlight-matches t
      grep-scroll-output t)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; match related
(show-paren-mode t)
(setq autopair-autowrap t)
(autopair-global-mode t)
;; (electric-pair-mode -1)
;; (setq electric-pair-pairs '(
;;                               (?\" . ?\")
;;                               (?\( . ?\))
;;                               (?\[ . ?\])
;;                               (?\{ . ?\})
;;                               ))

(add-hook 'prog-mode-hook
          '(lambda ()
             (require 'ffap)
             (require 'fic-mode)
             (turn-on-fic-mode)
             (rainbow-mode t)
             (rainbow-delimiters-mode t)
             (hs-minor-mode t)))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; spelling
(dolist (hook '(message-mode-hook
                ))
  (add-hook hook 'flyspell-mode))
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

(setq ispell-personal-dictionary "~/.emacs.d/dict-spell/.aspell.en.pws")
