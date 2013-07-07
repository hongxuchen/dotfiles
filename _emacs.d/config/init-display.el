(provide 'init-display)
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

;; comint
(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t)

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------

(menu-bar-mode -1) ;; use menu-bar since I am not familiar with the keybings, WTF
(tool-bar-mode -1)
;; (fringe-mode '(1 . 1))
(setq tool-bar-style 'image)
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))
(blink-cursor-mode -1)
(column-number-mode 1)
(setq winner-ring-size 20)
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

(if (display-graphic-p)
    (progn
      ;; (set-face-attribute 'default nil :font "Consolas 14")
      (set-face-attribute 'default nil :font "DejaVu Sans Mono 13")
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family "文泉驿等宽微米黑"
                                     :size 12
                                     :weight 'light)))))

;; WoMan settings
(setq woman-fill-column 90
      woman-use-own-frame nil)

;; man settings
(setq Man-notify-method 'aggressive
      Man-width 90
      Man-see-also-regexp "SEE ALSO\\|RELATED INFORMATION")

;; encodings
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)

(setq goto-address-mail-face 'link)
(add-hook 'find-file-hook 'goto-address-prog-mode)
