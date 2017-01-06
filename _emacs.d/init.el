; ------------------------------------------------------------------------------
;; general utilities
;; ------------------------------------------------------------------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(setq bdf-directory-list '("~/.font"))
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

(let ((path (shell-command-to-string ". ~/.zlocal; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; ------------------------------------------------------------------------------
;; configurations for specific features and modes
;; ------------------------------------------------------------------------------
(require 'cl)
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
(require 'init-compile)
(require 'init-keymaps)

;; major modes for editing
(require 'init-lisp)
(require 'init-markup)
(require 'init-cc-mode)
(require 'init-coq)
(require 'init-caml)
(require 'init-hs)
(require 'init-sh)

;; ------------------------------------------------------------------------------
;; Other issues
;; ------------------------------------------------------------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; (require 'time-date)
;; (server-start)
(setenv "LC_CTYPE" "zh_CN.UTF-8")
(recentf-open-files)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
