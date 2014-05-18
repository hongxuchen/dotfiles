(setq emacs-load-start-time (current-time))

(require 'cl)
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/config"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/doxymacs/share/emacs/site-lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/irony-mode/elisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/rtags/src"))

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
;; (setq *macbook-pro-support-enabled* t)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *linux-x* (and window-system *linux*) )

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

;; basic settings
;; (require 'scala-mode)
(require 'init-elpa)
(require 'init-misc)
(require 'my-utils)
(require 'init-dired)
(require 'init-display)
(require 'init-ibuffer)
(require 'init-minibuffer)
(require 'init-sessions)
(require 'init-editing)
(require 'init-git)
(require 'init-linum)
(require 'init-evil)
(require 'init-auto-insert)
(require 'init-modeline)
(require 'init-term)

;;----------------------------------------------------------------------------
;; writings
;;----------------------------------------------------------------------------
(require 'init-org)
(require 'init-markdown)
(require 'init-yasnippet)
(require 'init-tex)

;;----------------------------------------------------------------------------
;; development
;;----------------------------------------------------------------------------
(require 'init-compile)
(require 'init-auto-complete) ; after init-yasnippet to override TAB

(require 'init-cc-mode)
(require 'init-cmake)

(require 'init-python)
;; (require 'init-ruby)

(require 'init-lisp)
;; (require 'init-clojure)
;; (require 'init-common-lisp)

;; (require 'init-moz)
;; (require 'init-javascript)
(require 'init-nxml)

(require 'init-sh)
(require 'init-autoloads)
(require 'init-scratch)
(require 'init-keymaps)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(require 'time-date)
(load-persistent-scratch)
(message "startup time: %d seconds." (float-time (time-since emacs-load-start-time)))
(server-start)
(setenv "LC_CTYPE" "zh_CN.UTF-8")
