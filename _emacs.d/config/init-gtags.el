(defun my-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p")))
      (let ((olddir default-directory)
            (topdir (read-directory-name
                     "GTAGS: source tree root:" default-directory)))
        (cd topdir)
        (shell-command "gtags && echo 'GTAGS created'")
        (cd olddir))
    (shell-command "global -uv && echo 'GTAGS updated'")))

(defun my-add-gtagslibpath (libdir &optional del)
  "add external library directory to environment variable GTAGSLIBPATH.
gtags will concat that directory if needed.
C-u M-x `my-add-gtagslibpath' will remove the directory from GTAGSLIBPATH."
  (interactive "DDirectory containing GTAGS:\nP")
  (let (sl)
    (when (not (file-exists-p (concat (file-name-as-directory libdir) "GTAGS")))
        (let ((olddir default-directory))
          (cd libdir)
          (shell-command "gtags && echo 'GTAGS created'")
          (cd olddir)))
    (setq libdir (directory-file-name libdir))
    (setq sl (split-string (if (getenv "GTAGSLIBPATH") (getenv "GTAGSLIBPATH") "")  ":" t))
    (if del (setq sl (delete libdir sl)) (add-to-list 'sl libdir t))
    (setenv "GTAGSLIBPATH" (mapconcat 'identity sl ":"))))

(require 'xgtags)
(add-hook 'xgtags-mode-hook
          (lambda () (local-set-key "\C-cwU" 'my-gtags-create-or-update)))

(setq xgtags-support-mouse nil)
(setq xgtags-goto-tag 'unique)
(add-hook 'c-mode-common-hook (lambda () (xgtags-mode 1)))
(add-hook 'c++-mode-common-hook (lambda () (xgtags-mode 1)))

(provide 'init-gtags)
