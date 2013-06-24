(provide 'init-eshell)

(require 'em-prompt)
(require 'em-term)
(require 'em-cmpl)

;; (setq eshell-history-size 512)
;; (setq eshell-hist-ignoredups t)
;; (setq eshell-show-lisp-completions t)
;; (setq eshell-cmpl-expand-before-complete t)
;; (setq eshell-cmpl-cycle-completions t)
;; (add-hook 'eshell-mode-hook
;;           '(lambda () (define-key eshell-mode-map "\t" 'pcomplete-list)))

;; (setq eshell-scroll-to-bottom-on-input nil)
(setq eshell-scroll-to-bottom-on-output t)
(setq eshell-scroll-show-maximum-output t)

(setq eshell-cmpl-cycle-completions t
      eshell-save-history-on-exit t
      eshell-buffer-shorthand t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(setenv "PAGER" "cat")
(set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
(add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
          '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
(setq eshell-cmpl-cycle-completions t)

(add-to-list 'eshell-visual-commands "ssh")
(add-to-list 'eshell-visual-commands "tail")
(add-to-list 'eshell-command-completions-alist
             '("gunzip" "gz\\'"))
(add-to-list 'eshell-command-completions-alist
             '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "src")))

(defun eshell/cdl ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "lib")))

(defun eshell/cdg ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory ".git")))

(when (not (functionp 'eshell/find))
  (defun eshell/find (dir &rest opts)
    (find-dired dir (mapconcat (lambda (arg)
                                 (if (get-text-property 0 'escaped arg)
                                     (concat "\"" arg "\"")
                                   arg))
                               opts " "))))

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

(defun my-eshell-in-dir (&optional prompt)
  "Change the directory of an existing eshell to the directory of the file in
the current buffer or launch a new eshell if one isn't running. If the
current buffer does not have a file (e.g., a *scratch* buffer) launch or raise
eshell, as appropriate. Given a prefix arg, prompt for the destination
directory."
  (interactive "P")
  (let* ((name (buffer-file-name))
         (dir (cond (prompt (read-directory-name "Directory: " nil nil t))
                    (name (file-name-directory name))
                    (t nil)))
         (buffers (delq nil (mapcar (lambda (buf)
                                      (with-current-buffer buf
                                        (when (eq 'eshell-mode major-mode)
                                          (buffer-name))))
                                    (buffer-list))))
         (buffer (cond ((eq 1 (length buffers)) (first buffers))
                       ((< 1 (length buffers)) (ido-completing-read
                                                "Eshell buffer: " buffers nil t
                                                nil nil (first buffers)))
                       (t (eshell)))))
    (with-current-buffer buffer
      (when dir
        (eshell/cd (list dir))
        (eshell-send-input))
      (end-of-buffer)
      (pop-to-buffer buffer))))

(ansi-color-for-comint-mode-on)
(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
                              eshell-last-output-end))
(add-hook 'eshell-mode-hook
          '(lambda ()
             (add-to-list
              'eshell-output-filter-functions
              'eshell-handle-ansi-color)))

;;Here’s how to compile in the background
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

;; auto-complete
(defun ac-pcomplete ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (flet ((insert-and-inherit (&rest args)))
    ;; this code is stolen from `pcomplete' in pcomplete.el
    (let* (tramp-mode ;; do not automatically complete remote stuff
           (pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; note, buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. variables like $HOME may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; we auto-complete only if the stub is non-empty and matches
      ;; the end of the buffer text
      (when (and (not (zerop (length pcomplete-stub)))
                 (or (string= pcomplete-stub ; Emacs 23
                              (substring buftext
                                         (max 0
                                              (- (length buftext)
                                                 (length pcomplete-stub)))))
                     (string= pcomplete-stub ; Emacs 24
                              (substring arg
                                         (max 0
                                              (- (length arg)
                                                 (length pcomplete-stub)))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                  cnds))))))

(defvar ac-source-pcomplete
  '((candidates . ac-pcomplete)))

(add-hook 'eshell-mode-hook #'(lambda () (setq ac-sources '(ac-source-pcomplete))))
          (add-to-list 'ac-modes 'eshell-mode)
