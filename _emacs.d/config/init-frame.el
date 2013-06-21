(provide 'init-frame)
;;----------------------------------------------------------------------------
;; startup issues
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message
      (concat ";; " user-full-name "\n\n"))

(setq indicate-empty-lines t)

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------

(menu-bar-mode -1) ;; use menu-bar since I am not familiar with the keybings, WTF
(tool-bar-mode -1)
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))
(blink-cursor-mode -1)
(column-number-mode 1)
(winner-mode t)
(mouse-avoidance-mode 'banish)
(setq x-stretch-cursor t)

;; scroll related
;; (setq scroll-step 1)
;; (setq scroll-conservatively 10000)
;; (setq auto-window-vscroll nil)
(setq scroll-preserve-screen-position t)

;;----------------------------------------------------------------------------
;; face related
;;----------------------------------------------------------------------------
(require 'font-lock)
(global-font-lock-mode t)
(set-face-attribute 'default nil :height 125)
(require 'monokai-dark-theme)
(setq-default windmove-wrap-around t)

;; (if (display-graphic-p)
;;   ;; Setting English Font
;;   (set-face-attribute 'default nil :font "DejaVu Sans Mono 12")
;;   ;; Chinese Font
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                       charset (font-spec :family "WenQuanYi Micro Hei"
;;                                          :style=Regular
;;                                          :size=12)))
;;   )

;; nicer naming
(require 'uniquify) ;; 24.3 contained
(setq uniquify-buffer-name-style 'forward
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; WoMan settings
(setq woman-fill-column 100)
(setq woman-use-own-frame nil)

;; man settings
(setq Man-notify-method 'aggressive
      Man-width 90
      Man-see-also-regexp "SEE ALSO\\|RELATED INFORMATION")
