(defun my-doc-func ()
  (format "%s" (point)))
(define-minor-mode my-mode
  :lighter " my"
  (when (not (or
              (string= major-mode 'c-mode)
              (string= major-mode 'c++-mode)))
    (error "only works with C/C++"))
  (cond
   (my-mode
    (set (make-local-variable 'eldoc-documentation-function)
         'my-doc-func))
   (t
    )))
