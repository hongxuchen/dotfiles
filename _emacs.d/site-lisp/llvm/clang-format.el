;;   (global-set-key [C-M-tab] 'clang-format-region)

(require 'json)

(defcustom clang-format-binary "clang-format-3.4"
  "executable for clang-format")
(defcustom clang-format-binary "clang-format-diff-3.4"
  "python executable for clang-format-diff")

(defcustom clang-format-style "Google"
  "Style for clang format, candidates are \"LLVM\", \"Google\", \"Chromium\", \"Mozilla\""
    :type 'string)

(defun clang-format-region ()
  "Use clang-format to format the currently active region."
  (interactive)
  (let ((beg (if mark-active
                 (region-beginning) (line-beginning-position)))
        (end (if mark-active
                 (region-end) (line-end-position))))
    (clang-format beg end)))

(defun clang-format-buffer ()
  "Use clang-format to format the current buffer."
  (interactive)
  (clang-format (point-min) (point-max)))

(defun clang-format (begin end)
  "Use clang-format to format the code between BEGIN and END."
  (let* ((orig-windows (get-buffer-window-list (current-buffer)))
         (orig-window-starts (mapcar #'window-start orig-windows))
         (orig-point (point)))
    (unwind-protect
        (call-process-region (point-min) (point-max) clang-format-binary t t nil
                             "-offset" (number-to-string (1- begin))
                             "-length" (number-to-string (- end begin))
                             "-cursor" (number-to-string (1- (point)))
                             "-style" clang-format-style)
      (goto-char (point-min))
      (let ((json-output (json-read-from-string
                           (buffer-substring-no-properties
                             (point-min) (line-beginning-position 2)))))
        (delete-region (point-min) (line-beginning-position 2))
        (goto-char (1+ (cdr (assoc 'Cursor json-output))))
        (dotimes (index (length orig-windows))
          (set-window-start (nth index orig-windows)
                            (nth index orig-window-starts)))))))

(provide 'clang-format)
