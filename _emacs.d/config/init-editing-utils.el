;; Some basic preferences
(setq-default
 blink-cursor-delay 0
 blink-cursor-interval 0.4
 bookmark-default-file "~/.emacs.d/.bookmarks.el"
 buffers-menu-max-size 20
 case-fold-search t
 lazy-highlight-cleanup nil
 case-replace nil
 compilation-scroll-output t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 line-spacing 0.2
 mouse-yank-at-point nil
 set-mark-command-repeat-pop t
 tooltip-delay 0.5
 truncate-lines nil
 truncate-partial-width-windows nil
 visible-bell nil)

(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; other minor modes
(global-pointback-mode t)
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(setq kill-do-not-save-duplicates t)
(transient-mark-mode t)
(delete-selection-mode t)

;; M-x grep
(setq grep-program "grep"
      grep-command "grep -inH")

;; Don't disable narrowing commands
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

;; Get around the emacswiki spam protection
(eval-after-load 'oddmuse
  (add-hook 'oddmuse-mode-hook
            (lambda ()
              (unless (string-match "question" oddmuse-post)
                (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post))))))

(add-hook 'prog-mode-hook
          '(lambda ()
             (require 'ffap)
             (require 'fic-mode)
             (turn-on-fic-mode)
             (rainbow-mode t)
             (rainbow-delimiters-mode t)
             (hs-minor-mode t)))

;; which-func
(progn
  (require 'which-func)
  (which-function-mode t)
  (setq which-func-modes '(c-mode c++-mode python-mode makefile-mode sh-mode org-mode)))

;; don't show trailing whitespace in SQLi, inf-ruby etc.
(add-hook 'comint-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

(add-hook 'change-log-mode-hook 'turn-on-auto-fill)
(add-hook 'cc-mode-hook 'turn-on-auto-fill)

(provide 'init-editing-utils)
