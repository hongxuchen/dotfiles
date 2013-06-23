(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/config"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/doxymacs/share/emacs/site-lisp"))

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *spell-check-support-enabled* t)
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
(eval-when-compile (require 'cl))

;; basic settings
(require 'init-elpa)
(require 'init-basics)
(require 'my-utils)
(require 'init-misc)
(require 'init-frame)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-minibuffer)
(require 'init-sessions)
(require 'init-editing-utils)
(require 'init-git)
(require 'init-linum-mode)
(require 'init-evil)
(require 'init-symbols-tags)
(require 'init-auto-insert)
(require 'init-modeline)
;; (require 'init-smartparens)
;; (require 'init-mmm)
;; (require 'init-marmalade)  helper to marmalade
;; (require 'init-proxies)    ;; proxy settings
;; (require 'init-osx-keys)

;;----------------------------------------------------------------------------
;; writings
;;----------------------------------------------------------------------------
(require 'init-org)
(require 'init-markdown)
(require 'init-spelling)
(require 'init-yasnippet)

;;----------------------------------------------------------------------------
;; development
;;----------------------------------------------------------------------------

(require 'init-compile)
(require 'init-auto-complete) ; after init-yasnippet to override TAB
(require 'init-gtags) ;; gtags funs
(require 'xcscope)
(require 'init-flymake) ;; online check

(require 'init-cc-mode)
;; (require 'init-irony)
;; (require 'init-cmake-mode)

(require 'init-python-mode)
;; (require 'init-haskell)
;; (require 'init-ruby-mode)
;; (require 'init-rails)
;; (require 'init-lua-mode)

(require 'init-lisp)
;; (require 'init-slime)
;; (require 'init-clojure)
;; (require 'init-common-lisp)
;; (require 'init-uml)

;; (require 'init-moz)
;; (require 'init-javascript)
;; (require 'init-php)
(require 'init-nxml)
;; (require 'init-css)
;; (require 'init-haml)
;; (require 'init-zencoding-mode) ;behind init-better-register to override C-j

(require 'init-sh)
(require 'init-locales)
(require 'init-autoloads)
(require 'init-scratch)
(require 'init-keymaps)
(require 'init-eim)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'time-date)
(message "Emacs startup time: %d seconds."
(time-to-seconds (time-since emacs-load-start-time)))
(load-persistent-scratch)
(server-start)
