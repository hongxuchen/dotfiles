(require 'llvm-mode)

(defgroup llvmize nil
  "group for llvmize"
  :group 'llvm-mode)

(defcustom llvm-frontend "llvm-gcc"
  "the frontend to convert c to llvm IR assembly"
  :type 'string
  :group 'llvmize)

(defsubst llvmize-generate-flags ()
  (append ac-clang-flags '("-emit-llvm" "-S" "-o" "-")))

;; region related issues
(defun llvm-asm (&optional start end)
  "converting current buffer/region containing c/c++ source to LLVM assembly via its Frontend tool"
  (interactive)
  (let* ((start (if mark-active (region-beginning) (point-min)))
         (end (if mark-active (region-end) (point-max)))
         (outbuf (generate-new-buffer "*llvm-asm*")))
    (apply 'call-process-region start end "clang" nil outbuf nil (buffer-file-name) (llvmize-generate-flags))
    (switch-to-buffer outbuf)
    (llvm-mode)))

;; scan-build

(provide 'llvmize)
