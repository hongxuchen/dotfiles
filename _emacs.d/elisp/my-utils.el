(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "F")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (rename-file filename new-name t)
    (rename-buffer new-name t)
    (set-visited-file-name new-name)
    (set-buffer-modified-p nil)
    (message "new file: [%s]" new-name)))

(defmacro with-selected-frame (frame &rest forms)
  (let ((prev-frame (gensym))
        (new-frame (gensym)))
    `(progn
       (let* ((,new-frame (or ,frame (selected-frame)))
              (,prev-frame (selected-frame)))
         (select-frame ,new-frame)
         (unwind-protect
             (progn ,@forms)
           (select-frame ,prev-frame))))))

;;; ----------------------------------------------
(defvar paste-mode nil)
(add-to-list 'minor-mode-alist '(paste-mode " Paste"))
(defun paste-mode ()
  (interactive)
  (let ((buf (current-buffer))
        (paste-mode t))
    (with-temp-buffer
      (let ((stay t)
            (text (current-buffer)))
        (redisplay)
        (while stay
          (let ((char (let ((inhibit-redisplay t)) (read-event nil t 0.1))))
            (unless char
              (with-current-buffer buf (insert-buffer-substring text))
              (erase-buffer)
              (redisplay)
              (setq char (read-event nil t)))
            (cond
             ((not (characterp char)) (setq stay nil))
             ((eq char ?\r) (insert ?\n))
             ((eq char ?\e)
              (if (sit-for 0.1 'nodisp) (setq stay nil) (insert ?\e)))
             (t (insert char)))))
        (insert-buffer-substring text)))))

(defun my-goto-scratch-buffer ()
  "go to *scratch* buffer, create it if non-exist"
  (interactive)
  (let* ((scratch-name "*scratch*")
         (buf (get-buffer scratch-name)))
    (switch-to-buffer scratch-name)
    (unless buf
      (insert initial-scratch-message))
    (emacs-lisp-mode)))

(defun show-file-name () (interactive) (message (buffer-file-name)))

(defun my-gnu-indent ()
  "indent current c or header file using GNU indent"
  (interactive)
  (save-buffer)
  (shell-command (format "indent %s" (shell-quote-argument (buffer-file-name))))
  (revert-buffer t t t)
  (message "Successfully indented!"))

(defun autopep8-buffer ()
  (interactive)
  "auto format python buffer to be consistent with pep8 style"
  (basic-save-buffer)
  (call-process "autopep8" nil t nil "-i" (buffer-file-name))
  (normal-mode)
  )

(require 'clang-format)
(defun my-format-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (cond ((member major-mode '(makefile-mode makefile-gmake-mode org-mode))
         (message "will not cleanup buffer when major mode is %s" major-mode))
        ((member major-mode '(c-mode c++-mode))(clang-format-buffer))
        ((member major-mode '(python-mode)) (autopep8-buffer))
        (t (progn
             (untabify (point-min) (point-max))
             (indent-region (point-min) (point-max))
             )))
  (when (and (buffer-file-name) (buffer-modified-p)) (basic-save-buffer)))

(defvar switch-major-mode-history nil)
(defun switch-major-mode (mode)
  "Switch major mode.
It contains minor mode, but autoloads need to be included"
  (interactive
   (list
    (intern
     (completing-read "Switch to mode: "
                      obarray (lambda (s)
                                (and (fboundp s)
                                     (string-match "-mode$" (symbol-name s))))
                      t nil 'switch-major-mode-history))))
  (setq switch-major-mode-history (cons (symbol-name major-mode) switch-major-mode-history))
  (funcall mode))

(defun kill-process-interactive ()
  (interactive)
  (let ((pname (ido-completing-read "Process Name: "
                                    (mapcar 'process-name (process-list)))))
    (delete-process (get-process pname))))

(defun my-insert-date (&optional addTimeStamp-p)
  "Insert current date and or time.

• In this format yyyy-mm-dd.
• When called with `universal-argument', insert date and time, e.g. 2012-05-28T07:06:23-07:00
• Replaces text selection.

See also `current-date-time-string'."
  (interactive "P")
  (when (region-active-p) (delete-region (region-beginning) (region-end) ) )
  (cond
   ((equal addTimeStamp-p nil ) (insert (format-time-string "%Y-%m-%d")))
   (t (insert (current-date-time-string))) ) )

(defun current-date-time-string ()
  "Returns current date-time string in full ISO 8601 format.
Example: 「2012-04-05T21:08:24-07:00」.

Note, for the time zone offset, both the formats 「hhmm」 and 「hh:mm」 are valid ISO 8601. However, Atom Webfeed spec seems to require 「hh:mm」."
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (ξx) (format "%s:%s" (substring ξx 0 3) (substring ξx 3 5))) (format-time-string "%z")) ))

(defun count-string-matches (strn)
  "Return number of matches STRING following the point.
    Continues until end of buffer. Also display the count as a message."
  (interactive (list (read-string "Enter string: ")))
  (save-excursion
    (let ((count -1))
      (while
          (progn
            (setq count (1+ count))
            (search-forward strn nil t)))
      (message "%d matches" count)
      count)))

(defun my-indent-accoding-to-paren ()
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char)))
        (pos (point)))
    (save-excursion
      (cond ((string-match "[[{(<]" next-char)
             (indent-region pos (progn (forward-sexp 1) (point)) nil))
            ((string-match "[\]})>]" prev-char)
             (indent-region (progn (backward-sexp 1) (point)) pos nil))))))

(defun my-sudo-edit ()
  "Edit the current buffer file as superuser -
in a new buffer, that should look the same."
  (interactive)
  (let ((start-point-pos (point)))
    (let ((top-window-pos (point))
          (buffer (buffer-file-name)) )
      (kill-buffer)
      (find-file (format "/sudo::%s" buffer))
      (goto-char top-window-pos)
      (recenter-top-bottom 0)
      (goto-char start-point-pos) )))

(defun my-totd ()
  (interactive)
  (random t)
  (with-output-to-temp-buffer "*TOTD*"
    (let* ((commands (loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip for the day is:\n"
               "========================\n\n"
               (describe-function command)
               "\n\nInvoke with:\n\n"
               (with-temp-buffer
                 (where-is command t)
                 (buffer-string)))))))

(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (forward-char)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defmacro stante-after (feature &rest forms)
  "After FEATURE is loaded, evaluate FORMS.

FORMS is byte compiled.

FEATURE may be a named feature or a file name, see
`eval-after-load' for details."
  (declare (indent 1) (debug t))
  ;; Byte compile the body.  If the feature is not available, ignore warnings.
  ;; Taken from
  ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2012-11/msg01262.html
  `(,(if (if (symbolp feature)
             (require feature nil :no-error)
           (load feature :no-message :no-error))
         'progn
       (message "stante-after: cannot find %s" feature)
       'with-no-warnings)
    (eval-after-load ',feature
      `(funcall (function ,(lambda () ,@forms))))))

(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(provide 'my-utils)
