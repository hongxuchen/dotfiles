(setq evil-auto-indent t
      evil-cross-lines t
      evil-echo-state nil
      evil-ex-search-vim-style-regexp t
      evil-flash-delay 5
      evil-complete-all-buffers nil
      evil-ex-substitute-global t
      evil-want-visual-char-semi-exclusive t
      evil-move-cursor-back nil
      ;; evil-search-module 'isearch
      ;; evil-move-cursor-back t
      ;; evil-want-C-i-jump t
      ;; evil-want-fine-undo nil
      ;; evil-repeat-move-cursor t
      ;; evil-want-C-u-scroll nil
      ;; evil-shift-width 4
      ;; evil-lookup-func 'woman
      )
(require 'evil)
(evil-mode t)
(evil-set-initial-state 'text-mode 'normal)
(evil-set-initial-state 'prog-mode 'normal)
(evil-set-initial-state 'comint-mode 'emacs)
(evil-set-initial-state 'gud-mode 'emacs)
;; below are mode that derived from fundamental-mode
(evil-set-initial-state 'finder-mode 'emacs)
(evil-set-initial-state 'taglist-mode 'emacs)
(evil-set-initial-state 'xgtags-select-mode 'emacs) ;;xgtags select mode supports j/k originally
(evil-set-initial-state 'calendar-mode 'emacs) ;; evil treat this mode stupidly
(evil-set-initial-state 'archive-mode 'emacs) ;; evil treat this mode stupidly
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'wdired-mode 'normal)
(evil-set-initial-state 'jabber-roster-mode 'emacs)
(evil-set-initial-state 'org-agenda-mode 'emacs) ;; no editing
(evil-set-initial-state 'diff-mode 'emacs) ;; conflicts
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs) ;;term-mode is not a comint-mode!
(evil-set-initial-state 'shell-mode 'emacs)
(evil-set-initial-state 'speedbar-mode 'emacs)
(evil-set-initial-state 'Info-mode 'emacs)
(evil-set-initial-state 'recentf-dialog-mode 'normal)
(evil-set-initial-state 'douban-music-mode 'emacs)

;; derived from special-mode
(evil-set-initial-state 'ebrowse-tree-mode 'emacs)
(evil-set-initial-state 'ebrowse-member-mode 'emacs)
(evil-set-initial-state 'custom-theme-choose-mode 'emacs)
(evil-set-initial-state 'process-menu-mode 'emacs)
(evil-set-initial-state 'apt-utils-mode 'emacs)
(evil-set-initial-state 'ert-results-mode 'emacs)
(evil-set-initial-state 'rtags-mode 'emacs)

;; others
;; (evil-set-initial-state 'help-mode 'normal)
;; (evil-set-initial-state 'inf-ruby-mode 'emacs)
;; (evil-set-initial-state 'yari-mode 'emacs)

(when (display-graphic-p)
  (keyboard-translate ?\C-i ?\H-i)
  (define-key evil-motion-state-map [?\H-i] 'evil-jump-forward))
(evil-global-set-key 'normal (kbd "q") 'quit-window)
(evil-global-set-key 'normal (kbd "C-t") 'pop-global-mark)
(evil-global-set-key 'normal (kbd "K") 'man)
;; revert to emacs keymaps for some keys
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
(evil-global-set-key 'insert (kbd "<escape>") 'evil-normal-state)
(evil-global-set-key 'normal (kbd "g j") 'evil-next-line)
(evil-global-set-key 'normal (kbd "g k") 'evil-previous-line)
(evil-global-set-key 'normal (kbd "j") 'evil-next-visual-line)
(evil-global-set-key 'normal (kbd "k") 'evil-previous-visual-line)

;; M-n and <ESC> conflict
;; (evil-global-set-key 'insert (kbd "<ESC>") 'evil-normal-state)

(evil-global-set-key 'insert (kbd "M-n") 'evil-complete-next)
(evil-global-set-key 'insert (kbd "M-p") 'evil-complete-previous)

(defun evil-undefine ()
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))
(define-key evil-normal-state-map (kbd "<tab>") 'evil-undefine)

;; vim-surround like, @see https://github.com/timcharper/evil-surround
(require 'surround)
;; (add-hook 'org-mode-hook (lambda ()
;;                            (push '(?~ . ("~" . "~")) surround-pairs-alist)))
(global-surround-mode 1)

(require 'evil-leader)
(global-evil-leader-mode t)
(evil-leader/set-key
  "c" 'flymake-mode
  "r" 'review-fixme-comment
  "g" 'rgrep
  "f" 'my-format-buffer
  "s" 'show-file-name
  "b" 'magit-blame-mode
  "v" 'eval-buffer
  "t" 'taglist)

(provide 'init-evil)
