(provide 'init-compile)

;; smarter-compile
(require 'smarter-compile)
(autoload 'smarter-compile "smarter-compile" "smarter compile current file")

(setq compilation-window-height 8)
(setq compile-command "make "
      compile-history (list "make" "make clean"))
(setq compilation-read-command nil)
(setq compilation-finish-functions 'bury-compile-buffer-if-successful)

(defun bury-compile-buffer-if-successful (buf string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string= (buffer-name) "*compilation*")
       (string-match "finished" string)
       (not
        (with-current-buffer buf
          (goto-char (point-min))
          (search-forward "warning" nil t))))
      (progn
        ;; (bury-buffer buf)
        ;; (switch-to-prev-buffer (get-buffer-window buf) 'kill)
        (delete-other-windows)
        )))

(add-to-list 'auto-mode-alist
             (cons "Makefile"
                   'makefile-gmake-mode))

(append '(("CMakeLists\\.txt\\'" . cmake-mode)
          ("\\.cmake\\'" . cmake-mode))
        auto-mode-alist)
(autoload 'andersl-cmake-font-lock-activate "andersl-cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'andersl-cmake-font-lock-activate)
(add-to-list 'ac-modes 'cmake-mode)

;;; ninja
(defvar ninja-keywords)
(setq ninja-keywords
      (list
       '("^#.*" . font-lock-comment-face)
       (cons (concat "^" (regexp-opt '("rule" "build" "subninja" "include"
                                       "pool" "default")
                                     'words))
             font-lock-keyword-face)
       '("\\([[:alnum:]_]+\\) =" . (1 font-lock-variable-name-face))
       ;; Variable expansion.
       '("\\($[[:alnum:]_]+\\)" . (1 font-lock-variable-name-face))
       ;; Rule names
       '("rule \\([[:alnum:]_]+\\)" . (1 font-lock-function-name-face))
       ))
(define-derived-mode ninja-mode fundamental-mode "ninja"
  (setq comment-start "#")
  (setq font-lock-defaults '(ninja-keywords t))
  )

(add-to-list 'auto-mode-alist '("\\.ninja$" . ninja-mode))
