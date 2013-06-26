;; TODO
;; C-u to change quotes settings
;; google-region should not quote by default
;; misc search with google 'site:'
(defgroup google-this nil
  "Customization group for `google-this-mode'." :group 'help)
(defconst google-this-version "1.2.1"
  "Version string of the `google-this' package.")
(defconst google-this-version-int 2
  "Integer version number of the `google-this' package (for comparing versions).")
(defcustom google-wrap-in-quotes nil
  "If not nil, searches are wrapped in double quotes.

If a prefix argument is given to any of the functions, the
opposite happens."
  :type 'boolean
  :group 'google-this)

(defcustom google-this-suspend-after-search nil
  "Whether emacs should be minimized after a search is launched (calls `suspend-frame')."
  :type 'boolean
  :group 'google-this)

(define-prefix-command 'google-this-mode-submap)
(define-key google-this-mode-submap [return] 'google-search)
(define-key google-this-mode-submap "t" 'google-this) 
(define-key google-this-mode-submap "r" 'google-region) 
(define-key google-this-mode-submap "w" 'google-word)
(define-key google-this-mode-submap "s" 'google-symbol)
(define-key google-this-mode-submap "e" 'google-error) 
(define-key google-this-mode-submap "c" 'google-cpp-reference) 
(define-key google-this-mode-submap "L" 'google-translate-query-or-region)
(define-key google-this-mode-submap "g" 'google-github)
(define-key google-this-mode-submap "l" 'google-llvm)
(define-key google-this-mode-submap "f" 'google-file)
(define-key google-this-mode-submap "S" 'google-SO)

(defun google-translate-query-or-region ()
  "If region is active `google-translate-at-point', otherwise `google-translate-query-translate'."
  (interactive)
  (unless (functionp 'google-translate-at-point)
    (error "[google-this]: This command requires the 'google-translate' package."))
  (if (region-active-p)
      (call-interactively 'google-translate-at-point)
    (call-interactively 'google-translate-query-translate)))

(defcustom google-location-suffix "com"
  "The url suffix associated with your location (com, co.uk, fr, etc)."
  :type 'string
  :group 'google-this)

(defun google-url () "URL to google searches."
  (concat "https://www.google." google-location-suffix "/search?q=%s"))

(defun google-quoted-url () "URL to quoted google searches."
  (concat "https://www.google." google-location-suffix "/search?q=%22%s%22"))

(defcustom url-parser-regexps '(
                                ("%" "%25")
                                ("\\+" "%2B")
                                ("&" "%26")
                                ("\"" "%22")
                                ("/" "%2F")
                                ("\\\\" "\\\\\\\\")
                                ("[[:blank:]]+" "+")
                                )
  "List of (REGEXP REPLACEMENT) used by `parse-and-google-string'.

You shouldn't have to edit this. If you are forced to edit this
for some reason, contact me and let me know."
  :type '(repeat (list regexp string))
  :group 'google-this )

(defcustom google-error-regexp '(("^[^:]*:[0-9 ]*:\\([0-9 ]*:\\)? *" ""))
  "List of (REGEXP REPLACEMENT) pairs to parse error strings."
  :type '(repeat (list regexp string))
  :group 'google-this)

(defun google-decide-url (prefix)
  "Decide whether to quote or not."
  (if (if prefix (not google-wrap-in-quotes) google-wrap-in-quotes)
      (google-quoted-url)
    (google-url)))

;;;###autoload
(defun google-search (prefix)
  "Write and do a google search."
  (interactive "P")
  (let ((TEXT (replace-regexp-in-string
               "^\\s-+" ""
               (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (or (thing-at-point 'symbol)
                     (thing-at-point 'word)
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position)))) )))
    (setq TEXT (read-string (concat "Googling [" TEXT "]: ") nil nil TEXT))
    (if (stringp TEXT)
        (parse-and-google-string TEXT prefix)
      (message "[google-string] Empty query."))))

(defun parse-and-google-string (text prefix &optional url-decider)
  "Convert illegal characters in TEXT to their %XX versions, and then google."
  (unless url-decider (setq url-decider 'google-decide-url))
  (browse-url (replace-regexp-in-string
               "%s" 
               (dolist (rp url-parser-regexps text)
                 (setq text (replace-regexp-in-string
                             (car rp) (car (cdr rp)) text)))
               (funcall url-decider prefix)))
  (when google-this-suspend-after-search
    (suspend-frame)))

;;;###autoload
(defun google-string (prefix &optional TEXT NOCONFIRM)
  "Google given TEXT, but ask the user first if NOCONFIRM is nil."
  (unless NOCONFIRM
    (setq TEXT (read-string "Googling: " 
                            (if (stringp TEXT) (replace-regexp-in-string "^[[:blank:]]*" "" TEXT)))))
  (if (stringp TEXT)
      (parse-and-google-string TEXT prefix)
    (message "[google-string] Empty query.")))

;;;###autoload
(defun google-word (prefix)
  "Google the current word."
  (interactive "P")
  (google-string prefix (thing-at-point 'word) t))

;;;###autoload
(defun google-symbol (prefix)
  "Google the current symbol."
  (interactive "P")
  (google-string prefix (thing-at-point 'symbol) t))

;;;###autoload
(defun google-region (prefix)
  "Google the current region."
  (interactive "P")
  (google-string
   prefix (buffer-substring-no-properties (region-beginning) (region-end)) t))

;;;###autoload
(defun google-this (prefix)
  "Automatically decide what the user wants to google (always something under point).

Unlike `google-search' (which presents an empty prompt with
\"this\" as the default value), this function inserts the query
in the minibuffer to be edited."
  (interactive "P")
  (cond
   ((region-active-p) (google-region prefix))
   ((thing-at-point 'symbol) (google-string prefix (thing-at-point 'symbol)))
   ((thing-at-point 'word) (google-string prefix (thing-at-point 'word)))))

;;;###autoload
(defalias 'google 'google-this)

;;;###autoload
(defun google-error (prefix)
  "Google the current error in the compilation buffer."
  (interactive "P")
  (unless (boundp 'compilation-mode-map)
    (error "No compilation active."))
  (save-excursion
    (let ((buffer-name (next-error-find-buffer)))
      (unless (compilation-buffer-internal-p)
        (set-buffer buffer-name))
      (google-string prefix
                     (google-this-clean-error-string 
                      (buffer-substring (line-beginning-position) (line-end-position)))))))


;;;###autoload
(defun google-this-clean-error-string (s)
  "Parse error strings and turn them into googleable strings.

Removes unhelpful details like file names and line numbers from
simple error strings (such as c-like erros).

Uses replacements in `google-error-regexp' and stops at the first match."
  (interactive)
  (dolist (cur google-error-regexp out)
    (when (string-match (car cur) s)
        (setq out 
              (replace-regexp-in-string  (car cur)
                                         (car (cdr cur))
                                         s))
        (return out))))

;;;###autoload
(defun google-cpp-reference ()
  (interactive)
  (parse-and-google-string (concat "site:cppreference.com " (thing-at-point 'symbol)) nil 'google-feeling-lucky-decider))

;;;###autoload
(defun google-github ()
  (interactive)
  (parse-and-google-string (concat "site:github.com " (thing-at-point 'symbol)) nil))

;;;###autoload
(defun google-llvm ()
  (interactive)
  (parse-and-google-string (concat "site:llvm.org " (thing-at-point 'symbol)) nil))

;; TODO when region is active, choose it; otherwise prompt
;;;###autoload
(defun google-idioms ()
  (interactive)
  (parse-and-google-string (concat "site:idioms.thefreedictionary.com " (buffer-substring-no-properties (region-beginning) (region-end))) nil 'google-feeling-lucky-decider))

;;;###autoload
(defun google-SO ()
  (interactive)
  (parse-and-google-string (concat "site:stackoverflow.com " (thing-at-point 'symbol)) nil))

(defun google-feeling-lucky-decider (prefix)
  "Just returns the feeling lucky url."
  (concat "https://www.google." google-location-suffix "/search?btnI=I'm Feeling Lucky&q=%s"))

(defun google-file (file)
  "Use google to search for a file named FILE."
  (interactive "sSearch for file: ")
  (browse-url
   (concat "http://www.google.com/search?q="
            (concat "+intitle:\"index+of\" -inurl:htm -inurl:html -inurl:php "
                    file))))

;;;###autoload
(define-minor-mode google-this-mode nil nil " Google"
  `((,(kbd "C-c g") . ,google-this-mode-submap))
  :global t
  :group 'google-this)

(provide 'google-this)

;;; google-this.el ends here

;; Local Variables:
;; lexical-binding: t
;; End:
