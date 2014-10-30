;; ------------------------------------------------------------------------------
;; startup issues
;; ------------------------------------------------------------------------------
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message (concat ";; " user-full-name "\n\n")
      view-inhibit-help-message t)

;; ------------------------------------------------------------------------------
;; Window size and features
;; ------------------------------------------------------------------------------
(setq tool-bar-style 'image)
;; (fringe-mode '(1 . 1))
(blink-cursor-mode -1) ;; orginal enabled
(column-number-mode 1)
(setq winner-ring-size 20)
(winner-mode 1)
(mouse-avoidance-mode 'banish)
(setq x-stretch-cursor t)

;; ------------------------------------------------------------------------------
;; face related
;; ------------------------------------------------------------------------------
(require 'font-lock)
(global-font-lock-mode 1)
(set-face-attribute 'default nil :height 125)
(setq-default windmove-wrap-around t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/config")
(load-theme 'monokai-dark t)

(setq use-file-dialog nil
      use-dialog-box nil)
(if (display-graphic-p)
    (progn
      ;; (set-face-attribute 'default nil :font "Consolas 14")
      ;; (set-face-attribute 'default nil :font " Helvetica Neue  LT Std 14")
      ;; (set-face-attribute 'default nil :font "HelveticaNeueLT Std Ext 14")
      ;; (set-face-attribute 'default nil :font "HelveticaNeueLT Std Lt Cn 14")
      ;; (set-face-attribute 'default nil :font "HelveticaNeueLT Std Lt:style=45 Light")
      (set-face-attribute 'default nil :font "DejaVu Sans Mono 13")
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        ;; (set-fontset-font (frame-parameter nil 'font)
        ;;                   charset
        ;;                   (font-spec :family "文泉驿等宽微米黑"
        ;;                              :size 12
        ;;                              :weight 'light))
	))
  )

(progn
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1))

;; WoMan settings
(setq woman-fill-column 90
      woman-use-own-frame nil)

;; man settings
(setq Man-notify-method 'aggressive
      Man-width 90
      Man-see-also-regexp "SEE ALSO\\|RELATED INFORMATION")

;; ;; encodings
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; (setq locale-coding-system 'utf-8
;;       coding-system-for-read 'utf-8
;;       coding-system-for-write 'utf-8)

(setq goto-address-mail-face 'link)
(add-hook 'find-file-hook 'goto-address-prog-mode)

;;; ------------------------------------------------------------------------------
; line number
;; ------------------------------------------------------------------------------
(global-linum-mode 1)
(unless (window-system)
  (setq linum-format "%3d "))
(setq linum-mode-inhibit-modes-list
      '(eshell-mode
        help-mode
        Man-mode
        woman-mode
        compilation-mode
        calc-mode
        calc-trail-mode
        comint-mode
        inf-ruby-mode
        gud-mode
        term-mode
        gnus-group-mode
        gnus-summary-mode
        gnus-article-mode
        calendar-mode
        ))
(defadvice linum-on (around linum-on-inhibit-for-modes activate)
  "Stop the load of linum-mode for some major modes."
  (unless (member major-mode linum-mode-inhibit-modes-list)
    ad-do-it))

(provide 'init-display)
