(require 'cl)

;; ------------------------------------------------------------------------------
;; general utilities
;; ------------------------------------------------------------------------------
(dolist (path '("~/.emacs.d/config"
                "~/.linuxbrew/share/emacs/site-lisp"
                "~/.emacs.d/config/rtags/src"
                ))
  (add-to-list 'load-path (expand-file-name path)))

(setq *is-a-mac* (eq system-type 'darwin)
      *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac))
      *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns))
      *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux))
      *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix))
      *linux-x* (and window-system *linux*)
      )

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
(require 'init-keymaps)

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
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; (require 'time-date)
;; (load-persistent-scratch)
;; (server-start)
(setenv "LC_CTYPE" "zh_CN.UTF-8")
(recentf-open-files)
