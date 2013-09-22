(provide 'init-elpa)
;;------------------------------------------------------------------------------
;; ~/.emacs.d/site-lisp
;;------------------------------------------------------------------------------
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
(require 'bytecomp)
;;------------------------------------------------------------------------------
;; package.el configuration
;;------------------------------------------------------------------------------
(require 'package)

;; Patch up annoying package.el quirks
(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer))))

;; Add support to package.el for pre-filtering available packages

(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

(defadvice package--add-to-archive-contents
  (around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function', if non-nil."
  (when (or (null package-filter-function)
            (funcall package-filter-function
                     (car package)
                     (package-desc-vers (cdr package))
                     archive))
    ad-do-it))

(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE.
If the version is older than MIN-VERSION, update PACKAGE; if NO-REFRESH is
non-nil, refresh package contents to get the latest `package-archive-contents'"
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(defvar melpa-exclude-packages
  '(slime)
  "Don't install Melpa versions of these packages.")

;; Don't take Melpa versions of certain packages
(setq package-filter-function
      (lambda (package version archive)
        (and
         (not (memq package '(eieio)))
         (or (not (string-equal archive "melpa"))
             (not (memq package melpa-exclude-packages))))))

;;------------------------------------------------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;------------------------------------------------------------------------------

(package-initialize)
(require-package 'session)
(require-package 'smex)
(require-package 'w3m)
(require-package 'jabber)
(require-package 'autopair)
(require-package 'ibuffer-vc)
(require-package 'mmm-mode)
(require-package 'pointback)
(require-package 'undo-tree) ;; evil mode needs it!
(require-package 'ido-ubiquitous)
(require-package 'info+)
(require-package 'wgrep)
;; (require-package 'smartparens)
;; (require-package 'anything)
;; (require-package 'mic-paren) ;; extend paren.el
;; (require-package 'crontab-mode)
;; (require-package 'marmalade)
;; (require-package 'xml-rpc)
;; (require-package 'regex-tool)

;;------------------------------------------------------------------------------
;; vim like editing
;;------------------------------------------------------------------------------
(require-package 'evil)
(require-package 'evil-leader)
(require-package 'surround)

;;------------------------------------------------------------------------------
;; version control
;;------------------------------------------------------------------------------
;; git
(require-package 'magit)
(require-package 'github-browse-file)
(require-package 'git-commit-mode)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
;; (require-package 'magithub)
;; svn
;; (require-package 'dsvn)

;;------------------------------------------------------------------------------
;; writings
;;------------------------------------------------------------------------------
;; latex
(require-package 'auctex)
;; gnuplot
(require-package 'gnuplot)
;; markdown
(require-package 'markdown-mode)
;; org
(require-package 'cdlatex)
(require-package 'org-plus-contrib)
;; (require-package 'org-plus-contrib)
(require-package 'htmlize)

;;------------------------------------------------------------------------------
;; development
;;------------------------------------------------------------------------------
(require-package 'find-file-in-project)
;; jump requires which-func, which is built in
(require-package 'inflections) ;; required by jump
(require-package 'jump)
;; required by jump, surprisingly this package from elpa contains jump and inflections
(require-package 'findr)
(require-package 'flymake-cursor) ;;show flymake errors in minibuffer
;; snippets
(require-package 'dropdown-list)
(require-package 'yasnippet)

;; tags related

;; auto-complete
(require-package 'fuzzy)
(require-package 'pos-tip)
(require-package 'auto-complete) ;auto-complete depends on fuzzy

;; c/c++
(require-package 'cmake-mode)
(require-package 'cpputils-cmake)
;; (require-package 'cmake-mode) ;;debian/ubuntu actually has this

;; shell
(require-package 'flymake-shell)

;; python
(require-package 'elpy)

;; go
;; (require-package 'go-mode)

;; javascript
;; (require-package 'json)
;; (require-package 'js3-mode)
;; (require-package 'js2-mode)
;; (require-package 'js-comint)
;; (require-package 'flymake-jslint)

;; coffeescript
;; (require-package 'coffee-mode)
;; (require-package 'flymake-coffee)

;; php
;; (require-package 'php-mode)
;; (require-package 'flymake-php)

;; pages and styles
;; (require-package 'haml-mode)
;; (require-package 'flymake-haml)
;; (require-package 'sass-mode)
;; (require-package 'flymake-sass)
;; (require-package 'scss-mode)
;; (require-package 'less-css-mode)
(require-package 'pretty-mode)
;; (require-package 'smarty-mode) ;; html templates
;; (require-package 'zencoding-mode)
;; (require-package 'tidy)
;; (require-package 'flymake-css)
(require-package 'rainbow-mode)
(require-package 'rainbow-delimiters)

;;lua
;; (require-package 'lua-mode)
;; (require-package 'flymake-lua)

;; clojure
;; (require-package 'project-local-variables) ;;for clojure
;; (require-package 'elein)  ;;leiningen
;; (require-package 'clojure-mode)
;; (require-package 'clojure-test-mode)
;; (require-package 'slamhound) ;;clojure
;; (require-package 'cljsbuild-mode)
;; (require-package 'nrepl)
;; (require-package 'ac-nrepl)

;; ruby
(require-package 'yaml-mode)
;; (require-package 'ruby-mode)
;; (require-package 'robe)
;; (require-package 'inf-ruby)
;; (prequire-package 'yari)
;; (require-package 'rvm)   ;; rvm-open-gem to get gem's code
;; (require-package 'rinari) ; use latest rinari
;; (require-package 'ruby-compilation)
;; (require-package 'flymake-ruby)

;; erlang
;; (require-package 'erlang)

;; haskell
;; (require-package 'haskell-mode)

;; lisp
(require-package 'paredit)
(require-package 'slime)
(require-package 'slime-fuzzy)
(require-package 'slime-repl)
(require-package 'ac-slime)

(require-package 'sicp)

(require-package 's)

;;scala
;; (require-package 'scala-mode2)
(byte-recompile-directory "~/.emacs.d/site-lisp" 0)
