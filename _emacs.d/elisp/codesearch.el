(defcustom codesearch-csearch "csearch"
  "The name of the csearch program."
  :type '(string)
  :group 'codesearch)

(defcustom codesearch-cindex "cindex"
  "The name of the cindex program."
  :type '(string)
  :group 'codesearch)

(defcustom codesearch-csearchindex nil
  "CSEARCHINDEX environment variable value used when calling csearch."
  :type '(string)
  :group 'codesearch)

(defun codesearch-search (pattern)
  (interactive
   (list
    (read-string "Pattern: ")))
  (let ((process-environment (copy-alist process-environment))
        (switch-to-visible-buffer t))
    (setenv "CSEARCHINDEX" codesearch-csearchindex)
    (shell-command
     (format "%s -n %s" codesearch-csearch pattern)
     "*codesearch*"))
    (pop-to-buffer "*codesearch*")
    (compilation-mode))

(defun codesearch-search-at-point ()
  (interactive)
  (codesearch-search (thing-at-point 'word)))

(defun codesearch-build-index (dir)
  "Scan DIR to rebuild an index."
  (interactive
   (list
    (read-directory-name "Directory: ")))
  (let ((process-environment (copy-alist process-environment)))
    (setenv "CSEARCHINDEX" codesearch-csearchindex)
    (shell-command
     (message "%s %s" codesearch-cindex dir)
     (format "%s %s &" codesearch-cindex dir)
     "*codesearch*")))

(defun codesearch-update-index ()
  "Update an existing index."
  (interactive)
  (let ((process-environment (copy-alist process-environment)))
    (setenv "CSEARCHINDEX" codesearch-csearchindex)
    (shell-command
     (message "%s" codesearch-cindex)
     (format "%s &" codesearch-cindex)
     "*codesearch*")))

;;;###autoload(require 'codesearch)
(provide 'codesearch)
