(setq magit-save-some-buffers nil
      magit-process-popup-time 10
      magit-set-upstream-on-push 'askifnotset
      magit-highlight-whitespace nil
      magit-diff-refine-hunk t
      magit-completing-read-function 'magit-ido-completing-read)

(eval-after-load 'magit
  '(progn
     (defadvice magit-status (around magit-fullscreen activate)
       (window-configuration-to-register :magit-fullscreen)
       ad-do-it
       (delete-other-windows))
     (defun magit-quit-session ()
       "Restores the previous window configuration and kills the magit buffer"
       (interactive)
       (kill-buffer)
       (jump-to-register :magit-fullscreen))
     (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
     ;; (require 'magit-key-mode)
     (require 'magit-svn)))

(when *is-a-mac*
  (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))

(eval-after-load 'compile
  '(progn
     (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                         '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
       (add-to-list 'compilation-error-regexp-alist-alist defn))
     (dolist (defn '(git-svn-updated git-svn-needs-update))
       (add-to-list 'compilation-error-regexp-alist defn))))

(defvar git-svn--available-commands nil "Cached list of git svn subcommands")

(defun git-svn (dir)
  "Run git svn"
  (interactive "DSelect directory: ")
  (unless git-svn--available-commands
    (setq git-svn--available-commands
          (string-all-matches "^  \\([a-z\\-]+\\) +" (shell-command-to-string "git svn help") 1)))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn "
                     (ido-completing-read "git-svn command: " git-svn--available-commands nil t)))))

;; If using a "password = !some command" in .gitconfig, we need to
;; run the specified command to find the actual value
(defadvice gh-config (after my-maybe-execute-bang (key) activate)
  (when (and (string= key "password")
             (string-prefix-p "!" ad-return-value))
    (setq ad-return-value (shell-command-to-string (substring ad-return-value 1)))))

(setq magithub-use-ssl t)

;; -----------------------------------------------------------------------------
;; reverting issues
;; -----------------------------------------------------------------------------
(defvar my-vc-reverting nil
  "Whether or not VC or Magit is currently reverting buffers.")

(defadvice revert-buffer (after my-maybe-remove-elc activate)
  "If reverting from VC, delete any .elc file that will now be out of sync."
  (when my-vc-reverting
    (when (and (eq 'emacs-lisp-mode major-mode)
               buffer-file-name
               (string= "el" (file-name-extension buffer-file-name)))
      (let ((elc (concat buffer-file-name "c")))
        (when (file-exists-p elc)
          (message "Removing out-of-sync elc file %s" (file-name-nondirectory elc))
          (delete-file elc))))))

(defadvice magit-revert-buffers (around my-reverting activate)
  (let ((my-vc-reverting t))
    ad-do-it))

(defadvice vc-revert-buffer-internal (around my-reverting activate)
  (let ((my-vc-reverting t))
    ad-do-it))

(remove-hook 'find-file-hook  'vc-find-file-hook)
(setq vc-follow-symlinks t
      vc-stay-local t)

(provide 'init-vc)
