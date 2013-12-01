(append '(("CMakeLists\\.txt\\'" . cmake-mode)
          ("\\.cmake\\'" . cmake-mode))
        auto-mode-alist)
(autoload 'andersl-cmake-font-lock-activate "andersl-cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'andersl-cmake-font-lock-activate)
(add-to-list 'ac-modes 'cmake-mode)

(add-to-list 'auto-mode-alist '("\\.ninja$" . ninja-mode))
(autoload 'ninja-mode "ninja-mode")

(provide 'init-cmake)
