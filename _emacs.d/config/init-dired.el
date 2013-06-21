(provide 'init-dired)

(setq dired-recursive-copies t               ;copy recursively
      dired-recursive-deletes t              ;delete recursively
      dired-recursive-deletes 'always        ;delete confirm once
      dired-recursive-copies 'always         ;no confirm when copy
      dired-details-hidden-string "[ ... ] " ;chars denoting hidden info
      dired-listing-switches "-aluh"         ;args passed to `ls`
      directory-free-space-args "-Pkh"       ;dir free space arg
      dired-omit-size-limit nil              ;no omit size limit
      dired-dwim-target t)                   ;guess target dir
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                            auto-mode-alist))
(setq dired-auto-revert-buffer t)
(setq dired-no-confirm '(byte-compile chgrp chmod chown compress copy hardlink load move print shell symlink touch uncompress))
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-verbose nil)
(setq my-dired-special-files "")
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$\\|^GTAGS$\\|^GSYMS$\\|^GRTAGS$\\|^GPATH$" ))

(require 'openwith)
(openwith-mode t)
