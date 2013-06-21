(require 'autoinsert)
(setq file-template-insert-automatically t)
(setq auto-insert-query nil) ;;; no prompt before insertion
(add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)
(add-hook 'find-file-not-found-hooks #'auto-insert)

(provide 'init-auto-insert)

