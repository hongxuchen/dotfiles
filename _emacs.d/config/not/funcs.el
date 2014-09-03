(defun read-hidden-file (file arg)
  (interactive (list (read-file-name "choose a hidden file: " "~/" nil nil nil
                                     (lambda (name)
                                       (string-match "^\\." (file-name-nondirectory name))))
                     current-prefix-arg))
  (message "%S, %S" file arg))

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

;;file related
(with-current-buffer (find-file-noselect "/tmp/test")
  buffer-file-name)

(find-buffer-visiting "/tmp/test")
(get-file-buffer "/tmp/test")

(file-modes "/root/.bashrc")
(format "%o" (file-modes "/tmp"))

(message "%d" most-positive-fixnum)

(require 'calculator)
(let ((calculator-output-radix 'bin)
      (calculator-radix-grouping-mode nil))
  (calculator-number-to-string number))

(defun my-subseq (list from &optional to)
  (if (null to) (nthcdr from list)
    (butlast (nthcdr from list) (- (length list) to))))

(defun walk-path (dir action)
       "walk DIR executing ACTION with (dir file)"
       (cond ((file-directory-p dir)
              (or (char-equal ?/ (aref dir(1- (length dir))))
                  (setq dir (file-name-as-directory dir)))
              (let ((lst (directory-files dir nil nil t))
                     fullname file)
                (while lst
                  (setq file (car lst))
                  (setq lst (cdr lst))
                  (cond ((member file '("." "..")))
                        (t
                         (and (funcall action dir file)
                              (setq fullname (concat dir file))
                              (file-directory-p fullname)
                              (walk-path fullname action)))))))
             (t
              (funcall action
                       (file-name-directory dir)
                       (file-name-nondirectory dir)))))
    (defun walk-path-visitor (dir file)
       "Called by walk-path for each file found"
       (message (concat  dir file)))

    (walk-path "~/" 'walk-path-visitor)

(defun my-cool-box (title begin end)
  "Wrap the region with a cool box.
The result is like this:
,----------[ Title ]
| This is the marked region
| that will be boxed
`---------- "
  (interactive "sTitle: \nr")
  (setq end (copy-marker end t))
  (save-excursion
    (goto-char begin)
    (unless (looking-back "^")
      (insert "\n"))
    (insert ",----------[ ")
    (insert title)
    (insert " ]\n")
    (while (< (point) end)
      (insert "| ")
      (next-line)
      (beginning-of-line))
    (goto-char end)
    (unless (looking-back "^")
      (insert "\n"))
    (insert "`----------\n")))
