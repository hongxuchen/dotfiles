(require 'auto-complete)
(require 'yasnippet)

(defconst ac-clang-stdout "*ac-clang-stdout*")
(defconst ac-clang-stderr "/tmp/ac-clang-stderr")
(defcustom ac-clang-executable
  (executable-find "clang")
  "*Location of clang executable"
  :group 'auto-complete
  :type 'file)

(defcustom ac-clang-lang-option-function nil
  "Function to return the lang type for option -x.

Return value is a string that can be \"c\", \"c++\", \"objective-c\",\"objective-c++\", it
is a function that takes no arguments."
  :group 'auto-complete
  :type 'function)

(defcustom ac-clang-flags nil
  "Extra flags to pass to the Clang executable.
This variable will typically contain include paths, e.g., ( \"-I~/MyProject\", \"-I.\" )."
  :group 'auto-complete
  :type '(repeat (string :tag "Argument" "")))

(defvar ac-clang-precompiled-header nil)
(defun ac-clang-set-precompiled-header (ph)
  (interactive
   (let ((def (car (directory-files "." t "\\([^.]h\\|[^h]\\).pch\\'" t))))
     (list
      (read-file-name (concat "Clang precompiled header(current: " ac-clang-precompiled-header ") : ")
                      (when def (file-name-directory def))
                      def nil (when def (file-name-nondirectory def))))))
  (cond ((string-match "^[\s\t]*$" ph)
         (setq ac-clang-precompiled-header nil))
        (t
         (setq ac-clang-precompiled-header ph))))

(defun ac-clang-set-cflags ()
  "set new cflags for clang from input string"
  (interactive)
  (setq ac-clang-flags (split-string (read-string "New cflags: "))))

(defun ac-clang-set-cflags-from-shell-command ()
  "set new cflags for ac-clang from shell command output.
Usually it is something like \"pkg-config --cflags\", \"llvm-config --cflags\" etc."
  (interactive)
  (setq ac-clang-flags
        (split-string
         (shell-command-to-string
          (read-shell-command "Shell command: " nil nil
                              (and buffer-file-name
                                   (file-relative-name buffer-file-name)))))))

(defconst ac-clang-completion-pattern
  "^COMPLETION: \\(%s[^\s\n:]*\\)\\(?: : \\)*\\(.*$\\)")

;; in ac-clang-stdout buffer
(defun ac-clang-parse-output (prefix)
  (goto-char (point-min))
  (let ((pattern (format ac-clang-completion-pattern
                         (regexp-quote prefix)))
        lines match detailed_info
        (prev-match ""))
    (while (re-search-forward pattern nil t)
      (setq match (match-string-no-properties 1))
      (unless (string= "Pattern" match)
        (setq detailed_info (match-string-no-properties 2))

        (if (string= match prev-match)
            (progn
              (when detailed_info
                (setq match (propertize match
                                        'ac-clang-help
                                        (concat
                                         (get-text-property 0 'ac-clang-help (car lines))
                                         "\n"
                                         detailed_info)))
                (setf (car lines) match)
                ))
          (setq prev-match match)
          (when detailed_info
            (setq match (propertize match 'ac-clang-help detailed_info)))
          (push match lines))))
    lines))

(defun ac-clang-call-process (prefix &rest args)
  (let* (
         (buf (get-buffer-create ac-clang-stdout))
         (res))
    (with-current-buffer buf (erase-buffer))
    (setq res (apply 'call-process-region (point-min) (point-max)
                     ac-clang-executable nil (list ac-clang-stdout ac-clang-stderr) nil args))
    (unless (zerop res)
      (with-current-buffer (find-file-noselect ac-clang-stderr t)
        (goto-char (point-min))
        (if (re-search-forward "\\(?:error: \\(?:invalid\\|unknown\\)\\) argument:" nil t)
            (message "[clang] %s" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        ;; TODO more efficiently
        (kill-buffer)))
    (with-current-buffer buf
      (ac-clang-parse-output prefix))))

(defsubst ac-clang-build-location (pos)
  (save-excursion
    (goto-char pos)
    (format "%s:%d:%d" buffer-file-name (line-number-at-pos)
            (1+ (- (point) (line-beginning-position))))))

(defsubst ac-clang-lang-option ()
  (or (and ac-clang-lang-option-function
           (funcall ac-clang-lang-option-function))
      (cond ((eq major-mode 'c++-mode)
             "c++")
            ((eq major-mode 'c-mode)
             "c")
            ((eq major-mode 'objc-mode)
             (cond ((string= "m" (file-name-extension (buffer-file-name)))
                    "objective-c")
                   (t
                    "objective-c++")))
            (t
             "c++"))))

(defsubst ac-clang-build-complete-args (pos)
  (append '("-cc1" "-fsyntax-only")
          (list "-x" (ac-clang-lang-option))
          ac-clang-flags
          (when (stringp ac-clang-precompiled-header)
            (list "-include-pch" (expand-file-name ac-clang-precompiled-header)))
          (list "-code-completion-at" (ac-clang-build-location pos) buffer-file-name)))

(defsubst ac-clang-clean-document (s)
  ;; (when s
  (setq s (replace-regexp-in-string "<#\\|#>\\|\\[#" "" s))
  (setq s (replace-regexp-in-string "#\\]" " " s))
  ;; )
  s)

(defun ac-clang-document (item)
  (if (stringp item)
      (ac-clang-clean-document (get-text-property 0 'ac-clang-help item))))

(defface ac-clang-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for clang candidate"
  :group 'auto-complete)

(defface ac-clang-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the clang selected candidate."
  :group 'auto-complete)

;; (defsubst ac--in-string-comment ()
;;   "Return non-nil if point is in a literal (a comment or string)."
;;   (nth 8 (syntax-ppss)))

(defun ac-clang-candidate ()
  ;; (if (ac--in-string-comment)
  ;;     (message "in string/comment"))
  (and (buffer-modified-p)
       (let ((executing-kbd-macro t))
         (basic-save-buffer)))
  (save-restriction
    (widen)
    (apply 'ac-clang-call-process
           ac-prefix
           (ac-clang-build-complete-args (- (point) (length ac-prefix))))))

(defvar ac-template-start-point nil)
(defvar ac-template-candidates)

(defun ac-clang-action ()
  (let ((help (ac-clang-clean-document (get-text-property 0 'ac-clang-help (cdr ac-last-completion))))
        (raw-help (get-text-property 0 'ac-clang-help (cdr ac-last-completion)))
        (candidates (list)) ss fn args (ret-t "") ret-f)
    (setq ss (split-string raw-help "\n"))
    (dolist (s ss)
      (when (string-match "\\[#\\(.*\\)#\\]" s)
        (setq ret-t (match-string 1 s)))
      (setq s (replace-regexp-in-string "\\[#.*?#\\]" "" s))
      (cond ((string-match "^\\([^(]*\\)\\((.*)\\)" s)
             (setq fn (match-string 1 s)
                   args (match-string 2 s))
             (push (propertize (ac-clang-clean-document args) 'ac-clang-help ret-t
                               'raw-args args) candidates)
             (when (string-match "\{#" args)
               (setq args (replace-regexp-in-string "\{#.*#\}" "" args))
               (push (propertize (ac-clang-clean-document args) 'ac-clang-help ret-t
                                 'raw-args args) candidates))
             (when (string-match ", \\.\\.\\." args)
               (setq args (replace-regexp-in-string ", \\.\\.\\." "" args))
               (push (propertize (ac-clang-clean-document args) 'ac-clang-help ret-t
                                 'raw-args args) candidates)))
            ((string-match "^\\([^(]*\\)(\\*)\\((.*)\\)" ret-t) ;; check whether it is a function ptr
             (setq ret-f (match-string 1 ret-t)
                   args (match-string 2 ret-t))
             (push (propertize args 'ac-clang-help ret-f 'raw-args "") candidates)
             (when (string-match ", \\.\\.\\." args)
               (setq args (replace-regexp-in-string ", \\.\\.\\." "" args))
               (push (propertize args 'ac-clang-help ret-f 'raw-args "") candidates)))))
    (cond (candidates
           (setq candidates (delete-dups candidates))
           (setq candidates (nreverse candidates))
           (setq ac-template-candidates candidates)
           (setq ac-template-start-point (point))
           (ac-complete-template)

           (unless (cdr candidates) ;; unless length > 1
             (message (replace-regexp-in-string "\n" "   ;    " help)))) ;
          (t
           (message (replace-regexp-in-string "\n" "   ;    " help))))))

(defun ac-clang-prefix ()
  (or (ac-prefix-symbol)
      (let ((c (char-before)))
        (when (or (eq ?\. c)
                  ;; ->
                  (and (eq ?> c)
                       (eq ?- (char-before (1- (point)))))
                  ;; ::
                  (and (eq ?: c)
                       (eq ?: (char-before (1- (point))))))
          (point)))))

(ac-define-source clang
  '((candidates . ac-clang-candidate)
    (candidate-face . ac-clang-candidate-face)
    (selection-face . ac-clang-selection-face)
    (prefix . ac-clang-prefix)
    (requires . 0)
    (document . ac-clang-document)
    (action . ac-clang-action)
    (cache)
    (symbol . "cc")))

(defun ac-clang-same-count-in-string (c1 c2 s)
  (let ((count 0) (cur 0) (end (length s)) c)
    (while (< cur end)
      (setq c (aref s cur))
      (cond ((eq c1 c)
             (setq count (1+ count)))
            ((eq c2 c)
             (setq count (1- count))))
      (setq cur (1+ cur)))
    (= count 0)))

(defun ac-clang-split-args (s)
  (let ((sl (split-string s ", *")))
    (cond ((string-match "<\\|(" s)
           (let ((res (list)) (pre "") subs)
             (while sl
               (setq subs (pop sl))
               (unless (string= pre "")
                 (setq subs (concat pre ", " subs))
                 (setq pre ""))
               (cond ((and (ac-clang-same-count-in-string ?\< ?\> subs)
                           (ac-clang-same-count-in-string ?\( ?\) subs))
                      (push subs res))
                     (t
                      (setq pre subs))))
             (nreverse res)))
          (t
           sl))))

(defun ac-template-candidate ()
  ac-template-candidates)

(defun ac-template-action ()
  (unless (null ac-template-start-point)
    (let ((pos (point)) sl (snp "")
          (s (get-text-property 0 'raw-args (cdr ac-last-completion))))
      (cond ((string= s "")
             ;; function ptr call
             (setq s (cdr ac-last-completion))
             (setq s (replace-regexp-in-string "^(\\|)$" "" s))
             (setq sl (ac-clang-split-args s))
             (dolist (arg sl)
               (setq snp (concat snp ", ${" arg "}")))
             (yas-expand-snippet (concat "("  (substring snp 2) ")")
                                 ac-template-start-point pos))
            (t
             (unless (string= s "()")
               (setq s (replace-regexp-in-string "{#" "" s))
               (setq s (replace-regexp-in-string "#}" "" s))
               (setq s (replace-regexp-in-string "<#" "${" s))
               (setq s (replace-regexp-in-string "#>" "}" s))
               (setq s (replace-regexp-in-string ", \\.\\.\\." "}, ${..." s))
               (yas-expand-snippet s ac-template-start-point pos)
               ))))))

(defun ac-template-prefix ()
  ac-template-start-point)

(ac-define-source template
  '((candidates . ac-template-candidate)
    (prefix . ac-template-prefix)
    (requires . 0)
    (action . ac-template-action)
    (document . ac-clang-document)
    (cache)
    (symbol . "ct")))

(provide 'auto-complete-clang)

;;; auto-complete-clang.el ends here
