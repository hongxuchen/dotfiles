;;----------------------------------------------------------------------------
;; startup issues
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message
      (concat ";; " user-full-name "\n\n"))
(setq view-inhibit-help-message t)

(setq indicate-empty-lines t)

;; comint
(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t)

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------

(setq tool-bar-style 'image)
(fringe-mode '(1 . 1))
(blink-cursor-mode -1)
(column-number-mode 1)
(setq winner-ring-size 20)
(winner-mode t)
(mouse-avoidance-mode 'banish)
(setq x-stretch-cursor t)

;;----------------------------------------------------------------------------
;; face related
;;----------------------------------------------------------------------------
(require 'font-lock)
(global-font-lock-mode t)
(set-face-attribute 'default nil :height 125)
(setq-default windmove-wrap-around t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/elisp")
(load-theme 'monokai-dark t)

(if (display-graphic-p)
    (progn
      ;; (set-face-attribute 'default nil :font "Consolas 14")
      ;; (set-face-attribute 'default nil :font " Helvetica Neue  LT Std 14")
      ;; (set-face-attribute 'default nil :font "HelveticaNeueLT Std Ext 14")
      ;; (set-face-attribute 'default nil :font "HelveticaNeueLT Std Lt Cn 14")
      ;; (set-face-attribute 'default nil :font "HelveticaNeueLT Std Lt:style=45 Light")
      (set-face-attribute 'default nil :font "DejaVu Sans Mono 13")
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family "文泉驿等宽微米黑"
                                     :size 12
                                     :weight 'light))))
  (progn
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1))
  )

(defun switch-full-screen ()
      (interactive)
      (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

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

(provide 'init-display)
