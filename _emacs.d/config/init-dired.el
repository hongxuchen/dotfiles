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
(setq dired-omit-files "^\\.$\\|^\\.\\.$\\|^GTAGS$\\|^GSYMS$\\|^GRTAGS$\\|^GPATH$\\|\\.bc$|\\.o$\\|\\.out$\\|\\.aux$\\|\\.log$\\|\\.fdb_latexmk$\\|\\.fls$")

(require 'openwith)
(openwith-mode t)

(define-key dired-mode-map "W" 'wdired-change-to-wdired-mode)
(setq list-directory-brief-switches (purecopy "-ACF"))

(defvar dired-sort-map (make-sparse-keymap))
(define-key dired-mode-map "s" dired-sort-map)
(define-key dired-sort-map "s" (lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches " -S"))))
(define-key dired-sort-map "x" (lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches " -X"))))
(define-key dired-sort-map "t" (lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches " -t"))))
(define-key dired-sort-map "n" (lambda () "sort by Name" (interactive) (dired-sort-other dired-listing-switches)))
(define-key dired-sort-map "?" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name")))
