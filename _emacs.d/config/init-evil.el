;; these variables had better to be set before evil mode is loaded
(setq-default evil-auto-indent t
              evil-cross-lines t
              evil-echo-state nil
              evil-ex-search-vim-style-regexp t
              evil-flash-delay 5
              evil-complete-all-buffers nil ;; not sure
              evil-ex-substitute-global t
              ;; evil-search-module 'isearch
              ;; evil-move-cursor-back t
              ;; evil-want-C-i-jump t
              ;; evil-want-fine-undo nil
              ;; evil-repeat-move-cursor t
              ;; evil-want-C-u-scroll nil  ;; default
              ;; evil-shift-width 4
              ;; evil-lookup-func 'woman ;; default
              )
(require 'evil)
(evil-mode t)
(evil-set-initial-state 'text-mode 'normal)
(evil-set-initial-state 'prog-mode 'normal)
(evil-set-initial-state 'comint-mode 'emacs)
(evil-set-initial-state 'gud-mode 'emacs)
;; below are mode that derived from fundamental-mode
(evil-set-initial-state 'taglist-mode 'emacs)
(evil-set-initial-state 'xgtags-select-mode 'emacs) ;;xgtags select mode supports j/k originally
(evil-set-initial-state 'calendar-mode 'emacs) ;; evil treat this mode stupidly
(evil-set-initial-state 'archive-mode 'emacs) ;; evil treat this mode stupidly
(evil-set-initial-state 'dired-mode 'emacs) ;; seems that a lot of conflicts
(evil-set-initial-state 'wdired-mode 'normal) ;; seems that a lot of conflicts
(evil-set-initial-state 'jabber-roster-mode 'emacs) ;; evil conflicts with the keybings
(evil-set-initial-state 'org-agenda-mode 'emacs) ;; no editing
(evil-set-initial-state 'diff-mode 'emacs) ;; conflicts
(evil-set-initial-state 'eshell-mode 'emacs) ;;for simplicity
(evil-set-initial-state 'term-mode 'emacs) ;;term-mode is not a comint-mode!
(evil-set-initial-state 'shell-mode 'emacs)
(evil-set-initial-state 'speedbar-mode 'emacs)
(evil-set-initial-state 'Info-mode 'emacs) ;; the links
(evil-set-initial-state 'recentf-dialog-mode 'normal)
(evil-set-initial-state 'nav-mode 'emacs)

(evil-set-initial-state 'douban-music-mode 'emacs)
(evil-set-initial-state 'mpc-mode 'emacs)
(evil-set-initial-state 'mpc-status-mode 'emacs)
(evil-set-initial-state 'mpc-tagbrowser-mode 'emacs)
(evil-set-initial-state 'mpc-tagbrowser-dir-mode 'emacs)
(evil-set-initial-state 'mpc-songs-mode 'emacs)

;; derived from special-mode
(evil-set-initial-state 'ebrowse-tree-mode 'emacs)
(evil-set-initial-state 'ebrowse-member-mode 'emacs)
(evil-set-initial-state 'custom-theme-choose-mode 'emacs)
(evil-set-initial-state 'process-menu-mode 'emacs)
(evil-set-initial-state 'apt-utils-mode 'emacs)
(evil-set-initial-state 'ert-results-mode 'emacs)

;; others
;; (evil-set-initial-state 'help-mode 'normal)
;; (evil-set-initial-state 'inf-ruby-mode 'emacs)
;; (evil-set-initial-state 'yari-mode 'emacs)

;; revert to emacs keymaps for some keys
(evil-global-set-key 'normal (kbd "q") 'bury-buffer)
(evil-global-set-key 'normal (kbd "K") 'man)
(evil-global-set-key 'insert (kbd "\C-e") 'move-end-of-line)

;; vim-surround like, @see https://github.com/timcharper/evil-surround
(require 'surround)
;; (add-hook 'org-mode-hook (lambda ()
;;                            (push '(?~ . ("~" . "~")) surround-pairs-alist)))
(global-surround-mode 1)

(require 'evil-leader)
(global-evil-leader-mode t)
(setq evil-leader/leader "\\")
(evil-leader/set-key
  "r" 'review-fixme-comment
  "f" 'my-cleanup-buffer
  "T" 'nav-toggle
  "b" 'magit-blame-mode
  "t" 'taglist)

(provide 'init-evil)
