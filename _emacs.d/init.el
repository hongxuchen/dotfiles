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
;; (require 'init-coq)
;; (require 'init-caml)
;; (require 'init-hs)
(require 'init-tex)
(require 'init-sh)

;; ------------------------------------------------------------------------------
;; Other issues
;; ------------------------------------------------------------------------------
;; (setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file)
;; (require 'time-date)
;; (server-start)
(setenv "LC_CTYPE" "zh_CN.UTF-8")
(recentf-open-files)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (find-file-in-project deadgrep yaml-mode wgrep use-package smex session robe restart-emacs rainbow-mode rainbow-delimiters py-autopep8 pretty-mode popup-kill-ring pointback pcre2el paredit markdown-mode magit-svn latex-pretty-symbols latex-extra jump intero ido-completing-read+ ibuffer-vc htmlize hl-sexp graphviz-dot-mode github-browse-file flymake-shell flymake-cursor evil-surround evil-leader ensime elpy dired-k dash-at-point company-coq company-c-headers cdlatex autopair auctex-latexmk ag ac-math)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
