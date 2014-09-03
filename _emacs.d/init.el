(require 'cl)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/config"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/doxymacs/share/emacs/site-lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/irony-mode/elisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/rtags/src"))

;; ------------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;; ------------------------------------------------------------------------------
;; (setq *macbook-pro-support-enabled* t)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *linux-x* (and window-system *linux*) )

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

;; ------------------------------------------------------------------------------
;; Load configs for specific features and modes
;; ------------------------------------------------------------------------------

;; basics
(require 'init-elpa)
(require 'init-my-utils)
(require 'init-misc)
(require 'init-dired)
(require 'init-display)
(require 'init-ibuffer)
(require 'init-minibuffer)
(require 'init-editing)
(require 'init-git)
(require 'init-evil)
(require 'init-modeline)
(require 'init-electroic)
(require 'init-term)

;; development
(require 'init-lisp)
(require 'init-markup)
(require 'init-tex)
(require 'init-compile)
(require 'init-cc-mode)
(require 'init-python)
(require 'init-nxml)
(require 'init-sh)
(require 'init-autoloads)
;; (require 'init-ruby)
;; (require 'init-clojure)
;; (require 'init-common-lisp)
;; (require 'init-javascript)

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
