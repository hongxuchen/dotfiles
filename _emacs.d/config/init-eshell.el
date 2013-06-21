(provide 'init-eshell)

(require 'em-prompt)
(require 'em-term)
(require 'em-cmpl)

(setq eshell-history-size 512)
(setq eshell-hist-ignoredups t)

(setq eshell-cmpl-cycle-completions nil)
(add-hook 'eshell-mode-hook
           '(lambda () (define-key eshell-mode-map "\t" 'pcomplete-list)))

;; scroll to the bottom
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


(add-hook 'eshell-mode-hook '(lambda ()
                               (define-key eshell-mode-map (kbd "C-w") 'backward-kill-word)
                               ))

;; This will transform ansi color to faces in Emacs shell!
(ansi-color-for-comint-mode-on)
(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
                              eshell-last-output-end))
(add-hook 'eshell-mode-hook
          '(lambda ()
             (add-to-list
              'eshell-output-filter-functions
              'eshell-handle-ansi-color)))

;;Here’s how to compile in the background, also by Kai.
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

;;------------------------------------------------------------------------------------------
;; https://tsdh.wordpress.com/2013/05/31/eshell-completion-for-git-bzr-and-hg/
;;------------------------------------------------------------------------------------------
;;**** Git Completion
;;**** Git Completion

(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "available git commands in")
    (let (commands)
      (while (re-search-forward
	      "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
	      nil t)
	(push (match-string 1) commands)
	(when (match-string 2)
	  (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
	(push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
			     (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

;;**** Bzr Completion

(defun pcmpl-bzr-commands ()
  "Return the most common bzr commands by parsing the bzr output."
  (with-temp-buffer
    (call-process-shell-command "bzr" nil (current-buffer) nil "help" "commands")
    (goto-char 0)
    (let (commands)
      (while (re-search-forward "^\\([[:word:]-]+\\)[[:blank:]]+" nil t)
	(push (match-string 1) commands))
      (sort commands #'string<))))

(defconst pcmpl-bzr-commands (pcmpl-bzr-commands)
  "List of `bzr' commands.")

(defun pcomplete/bzr ()
  "Completion for `bzr'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-bzr-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-bzr-commands))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

;;**** Mercurial (hg) Completion

(defun pcmpl-hg-commands ()
  "Return the most common hg commands by parsing the hg output."
  (with-temp-buffer
    (call-process-shell-command "hg" nil (current-buffer) nil "-v" "help")
    (goto-char 0)
    (search-forward "list of commands:")
    (let (commands
	  (bound (save-excursion
		   (re-search-forward "^[[:alpha:]]")
		   (forward-line 0)
		   (point))))
      (while (re-search-forward
	      "^[[:blank:]]\\([[:word:]]+\\(?:, [[:word:]]+\\)*\\)" bound t)
	(let ((match (match-string 1)))
	  (if (not (string-match "," match))
	      (push (match-string 1) commands)
	    (dolist (c (split-string match ", ?"))
	      (push c commands)))))
      (sort commands #'string<))))

(defconst pcmpl-hg-commands (pcmpl-hg-commands)
  "List of `hg' commands.")

(defun pcomplete/hg ()
  "Completion for `hg'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-hg-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-hg-commands))
   (t
    (while (pcomplete-here (pcomplete-entries))))))
