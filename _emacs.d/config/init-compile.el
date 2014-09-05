;; smarter-compile
(require 'smarter-compile)
(autoload 'smarter-compile "smarter-compile" "smarter compile current file")

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
(setq compilation-window-height 8
      compilation-read-command nil
      compilation-finish-functions 'bury-compile-buffer-if-successful)
(setq compile-command "make "
      compile-history (list "make" "make clean"))

;; cmake
(append '(("CMakeLists\\.txt\\'" . cmake-mode)
          ("\\.cmake\\'" . cmake-mode))
        auto-mode-alist)
(autoload 'andersl-cmake-font-lock-activate "andersl-cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'andersl-cmake-font-lock-activate)

;; ninja -- managed by ninja-mode

(provide 'init-compile)
