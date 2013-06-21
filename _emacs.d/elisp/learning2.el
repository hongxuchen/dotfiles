(read-string "what your name? " user-full-name)

(defun read-hidden-file (file arg)
  (interactive (list (read-file-name "choose a hidden file: " "~/" nil nil nil
                                     (lambda (name)
                                       (string-match "^\\." (file-name-nondirectory name))))
                     current-prefix-arg))
  (message "%S, %S" file arg))

(defun foo () (interactive) (message "%S" current-prefix-arg ))

(defun tree-mapcar (func tree)
  (if (consp tree)
      (mapcar (lambda (child)
                (tree-mapcar func child))
              tree)
    (funcall func tree)))

(defmacro with-inhibit-read-only-t (&rest body)
  (declare (indent 0) (debug t))
  (cons 'let (cons '(inhibit-read-only t))
        body))

;;buffer related
(current-buffer)
(message (buffer-name))

(defun show-region (beg end)
  (interactive
   (if (or (null transient-mark-mode)
           mark-active)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (message "region start from %d to %d" beg end))

(defun mark-whole-sexp ()
  (interactive)
  (let ((bound (bounds-of-thing-at-point 'sexp)))
    (if bound
        (progn
          (goto-char (car bound))
          (set-mark (point))
          (goto-char (cdr bound)))
      (message "no sexp found at point!"))))

;; window related
(split-window)
(selected-window)
(selected-frame)
(selected-terminal)
(window-tree)

(setq foo (selected-window))
(delete-window)
(windowp foo)
(window-live-p foo)
(window-live-p (selected-window))

(window-height)
(window-width)
(window-edges)
(window-inside-edges)

;;file related
(with-current-buffer (find-file-noselect "/tmp/test")
  buffer-file-name)

(find-buffer-visiting "/tmp/test")
(get-file-buffer "/tmp/test")

(file-exists-p "/tmp/testt")
(file-readable-p "/tmp/test")
(format "%o" (file-modes "/tmp/test"))
(setq my-temp-file (make-temp-file "foo"))
(file-exists-p my-temp-file)

(message "%d" most-positive-fixnum)

(require 'calculator)
(let ((calculator-output-radix 'bin)
      (calculator-radix-grouping-mode nil))
  (calculator-number-to-string number))

(defun my-subseq (list from &optional to)
  (if (null to) (nthcdr from list)
    (butlast (nthcdr from list) (- (length list) to))))

(setq my-el-files (directory-files "~/.emacs.d/elisp" t "\\([^.]elc\\|[^h]\\).el\\'" t))
