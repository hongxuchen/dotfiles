;; -*- lexical-binding: nil; -*-
;; TODO
;; 1. goto-address-prog-mode to buttonize possible urls

(provide 'simple-dict)

(defgroup simple-dict nil
  "Customization group for `simpledict'." :group 'help)

(defcustom dict-pathname "dict"
  "Pathname of dict executable"
  :type 'string
  :group 'simple-dict)

(defcustom dict-server-host "localhost"
  "dictd server host (string); if nil, try to connect to dict.org"
  :type 'string
  :group 'simple-dict)

(defcustom dict-server-port nil
  "*dictd server port (string); if nil, use default port"
  :type 'string
  :group 'simple-dict)

(defcustom dict-db-name nil
  "*Dictionary database name - default is to browse all available databases.
Databases could be listed using M-x dict-list-databases"
  :type 'string
  :group 'simple-dict)

(defcustom dict-strategy nil
  "*Use specific matching strategy for dictionary lookup.
Default is to search for exact matches. Which strategies are available
depends on your dictd server. They could be listed using
M-x dict-list-strategies"
  :type 'string
  :group 'simple-dict)

(defconst dict-buffer-name "*simple-dict*"
  "Name of the buffer to display dictionary lookup results.")

(defcustom after-dict-hook nil
  "hooks that runs after dict function")

(setq after-dict-hook '(shell-command (concat "dict-emacs '" (format (shell-quote-argument word-to-lookup)) "'") ))

(defconst dict-options-alist
  '((dict-server-host    "-h" t)
    (dict-server-port    "-p" t)
    (dict-db-name        "-d" t)
    (dict-db-list        "-D" nil)
    (dict-strategy       "-s" t)
    (dict-strategy-list  "-S" nil)
    (dict-server-info    "-I" nil))
  "Dict client options. It is processed as an alist keyed by command name,
third value in each row shows if correspondent option requires additional
argument (which is the value of a variable named the same as the command).")

(defun dict-make-options-list (options)
  "Make list of options for dict"
  (let ((list))
    (dolist (cmd options (nreverse list))
      (let ((opt (assoc cmd dict-options-alist)))
        (if (not (caddr opt))       ; Option doesn't require additonal
            (push (cadr opt) list)  ;  parameter
          (when (symbol-value cmd)  ; Only if the additional parameter's value
            (push (cadr opt) list)  ;  is not nil
            (push (symbol-value cmd) list)))))))

(defmacro dict-client-call (options &rest args)
  "Construct a body of dict- function using required options and arguments"
  `(with-output-to-temp-buffer dict-buffer-name
     (apply #'call-process (nconc (list dict-pathname nil dict-buffer-name nil)
                                  (dict-make-options-list ,options)
                                  ,@args))))

(defun dict-list-databases ()
  "List dictionary databases available on default server"
  (interactive)
  (dict-client-call '(dict-server-host dict-server-port dict-db-list) '()))

(defun dict-list-strategies ()
  "List dictionary lookup strategies available on default server"
  (interactive)
  (dict-client-call '(dict-server-host dict-server-port dict-strategy-list) '()))

(defun dict-server-information ()
  "Show information about dictd server"
  (interactive)
  (dict-client-call '(dict-server-host dict-server-port dict-server-info) '()))

;; TODO when region is selected
(defun dict-lookup-definition (&optional manual)
  "Look up the word definition using dict client-server protocol.
1. When the region is active, the whole region would be treated as the phrase to
be searched.
2. Otherwise the default word is the one nearby(if any).
3. Use `\\[universal-argument]' to search the definition of the word/phrase you
specify."
  (interactive "P")
  (let ((word-to-lookup))
    (if (region-active-p)
        (setq word-to-lookup
              (buffer-substring-no-properties
               (region-beginning) (region-end)))
      (progn (setq word-to-lookup (current-word nil t))
             (unless
                 (and (equal manual nil) word-to-lookup
                      (string-match-p "^[A-Za-z]+$" word-to-lookup))
               (setq word-to-lookup
                     (read-string
                      (concat "Look up word[" (current-word nil t) "]:")
                      nil nil word-to-lookup)))))
    (dict-client-call '(dict-server-host
                        dict-server-port
                        dict-db-name
                        dict-strategy)
                      (list word-to-lookup))
    (unless (string= (buffer-name (current-buffer)) dict-buffer-name)
      (switch-to-buffer-other-window dict-buffer-name))
    (unless (search-forward "No definitions found for" nil t)
        (write-region (concat word-to-lookup "\n") nil (expand-file-name (format-time-string "%Y%m") "~/.dict") t))
    ))
