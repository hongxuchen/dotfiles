(require 'autoinsert)
(setq file-template-insert-automatically t)
(setq auto-insert-query nil) ;;; no prompt before insertion
(add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)
(add-hook 'find-file-hooks 'auto-insert)
(define-auto-insert 'sh-mode '(nil "#!/bin/bash\n\n"))
(define-auto-insert 'python-mode '(nil "#!/usr/bin/python\n\n"))
(provide 'init-auto-insert)
