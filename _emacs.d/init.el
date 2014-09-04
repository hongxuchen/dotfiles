(require 'cl)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/config"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/config/doxymacs/share/emacs/site-lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/config/irony-mode/elisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/config/rtags/src"))

;; ------------------------------------------------------------------------------
;; general utilities
;; ------------------------------------------------------------------------------
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *linux-x* (and window-system *linux*) )

;; ------------------------------------------------------------------------------
;; configurations for specific features and modes
;; ------------------------------------------------------------------------------

;; basics
(require 'init-elpa)
(require 'init-my-utils)
(require 'init-misc)
(require 'init-files)
(require 'init-display)
(require 'init-minibuffer)
(require 'init-editing)
(require 'init-vc)
(require 'init-evil)
(require 'init-modeline)
(require 'init-electronic)
(require 'init-comint)
(require 'init-compile)

;; major modes for editing
(require 'init-lisp)
(require 'init-markup)
(require 'init-tex)
(require 'init-cc-mode)
(require 'init-python)
(require 'init-nxml)
(require 'init-sh)

;; ------------------------------------------------------------------------------
;; Other issues
;; ------------------------------------------------------------------------------
(require 'init-keymaps)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(require 'time-date)
;; (load-persistent-scratch)
(server-start)
(setenv "LC_CTYPE" "zh_CN.UTF-8")
(recentf-open-files)
