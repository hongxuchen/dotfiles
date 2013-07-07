(provide 'init-eshell)

(require 'em-prompt)
(require 'em-term)
(require 'em-cmpl)

(ansi-color-for-comint-mode-on)
(setq eshell-save-history-on-exit t
      eshell-history-size 512
      eshell-hist-ignoredups t
      eshell-cmpl-ignore-case t
      eshell-cp-interactive-query nil
      eshell-ln-interactive-query nil
      eshell-mv-interactive-query nil
      eshell-rm-interactive-query t
      eshell-mv-overwrite-files t
      eshell-highlight-prompt   t
      eshell-show-lisp-completions t
      eshell-cmpl-expand-before-complete t
      eshell-cmpl-cycle-completions t
      eshell-scroll-to-bottom-on-input nil
      eshell-scroll-to-bottom-on-output t
      eshell-scroll-show-maximum-output t
      eshell-save-history-on-exit t
      eshell-buffer-shorthand t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(setenv "PAGER" "cat")
(set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
(add-hook 'eshell-mode-hook
          '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))

(add-to-list 'eshell-visual-commands "ssh")
(add-to-list 'eshell-visual-commands "tail")
(add-to-list 'eshell-command-completions-alist '("gunzip" "gz\\'"))
(add-to-list 'eshell-command-completions-alist '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

(defun eshell/ii (file)(ido-find-file file))
(defun eshell/ed (file1 file2)(ediff-files file1 file2))
(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "src")))

(defun eshell/cdl ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "lib")))

(defun eshell/cdg ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory ".git")))

(when (not (functionp 'eshell/rgrep))
  (defun eshell/rgrep (&rest args)
    "Use Emacs grep facility instead of calling external grep."
    (eshell-grep "rgrep" args t)))

(defun eshell/extract (file)
  (let ((command (some (lambda (x)
                         (if (string-match-p (car x) file)
                             (cadr x)))
                       '((".*\.tar.bz2" "tar xjf")
                         (".*\.tar.gz" "tar xzf")
                         (".*\.bz2" "bunzip2")
                         (".*\.rar" "unrar x")
                         (".*\.gz" "gunzip")
                         (".*\.tar" "tar xf")
                         (".*\.tbz2" "tar xjf")
                         (".*\.tgz" "tar xzf")
                         (".*\.zip" "unzip")
                         (".*\.Z" "uncompress")
                         (".*" "echo 'Could not extract the file:'")))))
    (eshell-command-result (concat command " " file))))

(defface my-eshell-error-prompt-face
  '((((class color) (background dark)) (:foreground "red" :bold t))
    (((class color) (background light)) (:foreground "red" :bold t)))
  "Face for nonzero prompt results"
  :group 'eshell-prompt)

(add-hook 'eshell-after-prompt-hook
          (defun my-eshell-exit-code-prompt-face ()
            (when (and eshell-last-command-status
                       (not (zerop eshell-last-command-status)))
              (let ((inhibit-read-only t))
                (add-text-properties
                 (save-excursion (beginning-of-line) (point)) (point-max)
                 '(face my-eshell-error-prompt-face))))))

(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
                              eshell-last-output-end))
(add-hook 'eshell-mode-hook
          '(lambda ()
             (add-to-list
              'eshell-output-filter-functions
              'eshell-handle-ansi-color)))

(defun eshell/ec (&rest args)
  "Use `compile' to do background makes."
  (if (eshell-interactive-output-p)
      (let ((compilation-process-setup-function
             (list 'lambda nil
                   (list 'setq 'process-environment
                         (list 'quote (eshell-copy-environment))))))
        (compile (eshell-flatten-and-stringify args))
        (pop-to-buffer compilation-last-buffer))
    (throw 'eshell-replace-command
           (let ((l (eshell-stringify-list (eshell-flatten-list args))))
             (eshell-parse-command (car l) (cdr l))))))
(put 'eshell/ec 'eshell-no-numeric-conversions t)

(defvar ac-source-eshell-pcomplete
  '((candidates . (pcomplete-completions))))
(defun ac-complete-eshell-pcomplete ()
  (interactive)
  (auto-complete '(ac-source-eshell-pcomplete)))
(add-to-list 'ac-modes 'eshell-mode)
(setq ac-sources '(ac-source-eshell-pcomplete))
