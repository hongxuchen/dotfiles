(defvar taglist-mode-hook nil)

(defface taglist-function-face
  '((((class color) (background dark))
     (:foreground "#11ff33"))
    (((class color) (background light))
     (:foreground "#ee1111"))
    (t (:bold t)))
  "Face used to highlight function name in the *cscope* buffer."
  :group 'taglist)

(defvar taglist-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'taglist-jump)
    (define-key map (kbd "q") 'taglist-kill)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "h") 'backward-char)
    (define-key map (kbd "l") 'forward-char)
    map))

(defvar taglist-window nil)
(defvar taglist-current 0)
(defconst taglist-buffer "*TAGLIST*")

;;;###autoload
(defun taglist ()
  (interactive)
  (require 'speedbar)
  ;; (require 'imenu)

  ;; Clear cache
  ;; (setq imenu--index-alist nil)

  (let ((source-buffer (current-buffer))
        (current-line (line-number-at-pos)))

    (if (get-buffer taglist-buffer)
        (kill-buffer taglist-buffer))
    (set-buffer (get-buffer-create taglist-buffer))

    ;; Call speedbar tags
    (setq taglist-current 0)
    (taglist-fill-tags
     source-buffer
     (cddr (speedbar-fetch-dynamic-tags
            (buffer-file-name source-buffer)))
     ""
     current-line)

    (goto-char (point-min))
    (forward-line (1- taglist-current))

    (setq taglist-window (split-window-horizontally))
    (set-window-buffer taglist-window taglist-buffer)
    (select-window taglist-window)
    (taglist-mode))
  (setq buffer-read-only t))

(defun taglist-fill-tags (source-buffer tags prefix current)
  (while tags
    (if (integer-or-marker-p (cdar tags))
        (let ((tag-line
               (with-current-buffer source-buffer
                 (line-number-at-pos (cdar tags)))))
          (setq my-tagline (format "\t%s L%-5d%s%s\n"
                          (buffer-name source-buffer)
                          tag-line
                          prefix
                          (caar tags)))
          (put-text-property 0 (length my-tagline) 'font-lock-face 'taglist-function-face my-tagline)
          (insert my-tagline)
          (when (>= current tag-line)
            (setq taglist-current
                  (1+ taglist-current))))
      (let* ((dir-string (caar tags))
             (marker (get-text-property 0 'org-imenu-marker dir-string))
             (tag-line 0))
        (if marker
            (setq tag-line
                  (with-current-buffer source-buffer
                    (line-number-at-pos marker))))
        (insert (format "\t%s L%-5d%s%s\n"
                        (buffer-name source-buffer)
                        tag-line
                        prefix
                        (caar tags)))
        (when (>= current tag-line)
          (setq taglist-current
                (1+ taglist-current)))
        (taglist-fill-tags source-buffer
                           (cdar tags)
                           (concat "+-" prefix)
                           current)))
    (setq tags (cdr tags))))

(defun taglist-kill ()
  (interactive)
  (if (and taglist-window
           (window-live-p taglist-window)
           (not (one-window-p)))
      (delete-window taglist-window))
  (setq taglist-window nil)
  ;; (bury-buffer taglist-buffer)
  )

(defun taglist-jump ()
  (interactive)
  (let ((line (buffer-substring
               (line-beginning-position)
               (line-end-position))))
    (string-match "^\t\\([^ ]*\\) L\\([0-9]+\\)[^0-9]" line)
    (taglist-kill)
    (switch-to-buffer (match-string 1 line))
    (goto-char (point-min))
    (forward-line (1- (string-to-number (match-string 2 line))))))

;;;###autoload
(defun taglist-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map taglist-map)
  (setq major-mode 'taglist-mode)
  (setq mode-name "TagList")
  (run-mode-hooks 'taglist-mode-hook))

(provide 'taglist)

;; Local Variables:
;; lexical-binding: t
;; End:
