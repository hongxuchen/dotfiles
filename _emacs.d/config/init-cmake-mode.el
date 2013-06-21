(require 'cmake-mode)
(setq auto-mode-alist (append '(("CMakeLists\\.txt\\'" . cmake-mode))
                              '(("\\.cmake\\'" . cmake-mode))
                              auto-mode-alist))

;; (add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
(provide 'init-cmake-mode)
