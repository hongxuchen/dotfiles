(require 'llvm-mode)

(defgroup llvmize nil
  "group for llvmize"
  :group 'llvm-mode)

(defcustom llvm-frontend "clang"
  "the frontend to convert c to llvm IR assembly"
  :type 'string
  :group 'llvmize)

(defvar irony-compile-flags)
(defsubst llvmize-generate-flags ()
  (append irony-compile-flags '("-emit-llvm" "-S" "-o" "-")))

;; region related issues
(defun llvm-asm (&optional start end)
  "converting current buffer/region containing c/c++ source to LLVM assembly via its Frontend tool"
  (interactive)
  (let* ((start (if mark-active (region-beginning) (point-min)))
         (end (if mark-active (region-end) (point-max)))
         (outbuf (generate-new-buffer "*llvm-asm*"))
         (res))
    (setq res (apply 'call-process-region start end llvm-frontend nil outbuf nil (buffer-file-name) (llvmize-generate-flags)))
    (if (zerop res)
        (progn
          (switch-to-buffer-other-window outbuf)
          (llvm-mode))
      (switch-to-buffer-other-window outbuf))))

(defun clang-asm (&optional start end)
  (interactive)
  (let ((llvm-frontend "clang"))
    (llvm-asm start end))
  )

;; scan-build

(provide 'llvmize)


;; TODO
;; 1. opt integeration, also possible to view differences between each pass
