;; ------------------------------------------------------------------------------
;; ~/.emacs.d/site-lisp
;; ------------------------------------------------------------------------------
(if (fboundp 'normal-top-level-add-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path
              (append
               (loop for dir in (directory-files my-lisp-dir)
                     unless (string-match "^\\." dir)
                     collecting (expand-file-name dir))
               load-path)))))

;; ------------------------------------------------------------------------------
;; package.el configuration
;; ------------------------------------------------------------------------------
(require 'package)
(package-initialize)

(defun require-package (package &optional no-refresh)
  (if (package-installed-p package)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package t)))))

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; ------------------------------------------------------------------------------
;; general
;; ------------------------------------------------------------------------------

(require-package 'session)
(require-package 'smex)
(require-package 'dired-k)
(require-package 'autopair)
(require-package 'ibuffer-vc)
(require-package 'pointback)
(require-package 'undo-tree) ;; evil mode needs it!
(require-package 'ido-ubiquitous)
(require-package 'info+)
(require-package 'wgrep)
(require-package 'popup-kill-ring)
(require-package 's)
(require-package 'robe)
(require-package 'inf-ruby)

;; ------------------------------------------------------------------------------
;; vim like editing
;; ------------------------------------------------------------------------------
(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-surround)

;;------------------------------------------------------------------------------
;; version control
;;------------------------------------------------------------------------------
;; git
(require-package 'magit)
(require-package 'magit-svn)
(require-package 'github-browse-file)
(require-package 'git-commit-mode)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)

(require-package 'dash-at-point)

;;------------------------------------------------------------------------------
;; writings
;;------------------------------------------------------------------------------
(require-package 'auctex)
(require-package 'auctex-latexmk)
(require-package 'ac-math)
(require-package 'latex-extra)
(require-package 'latex-pretty-symbols)
(require-package 'markdown-mode)
;; org
(require-package 'cdlatex)
;; (require-package 'org-plus-contrib)
(require-package 'htmlize)
(require-package 'graphviz-dot-mode)

;;------------------------------------------------------------------------------
;; development
;;------------------------------------------------------------------------------
(require-package 'find-file-in-project)
(require-package 'inflections) ;; required by jump
(require-package 'jump)
;; required by jump, surprisingly this package from elpa contains jump and inflections
(require-package 'findr)
(require-package 'flymake-cursor) ;;show flymake errors in minibuffer
;; snippets
(require-package 'dropdown-list)
(require-package 'yasnippet)

;; ac
(require-package 'pos-tip)
                                        ; company-mode
;; (require-package 'company-mode)
(require-package 'company-c-headers)

;; shell
(require-package 'flymake-shell) ;; require flymake-easy

;; python
(require-package 'elpy)
(require-package 'py-autopep8)

(require-package 'pretty-mode)

(require-package 'rainbow-mode)
(require-package 'rainbow-delimiters)

(require-package 'yaml-mode)

;; lisp
(require-package 'hl-sexp)
(require-package 'paredit)

(require-package 's)

(byte-recompile-directory "~/.emacs.d/site-lisp" 0)

(provide 'init-elpa)
