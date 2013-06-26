(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)

;; Various preferences
(setq org-log-done 'time
      org-descriptive-links nil
      org-completion-use-ido t
      org-edit-src-content-indentation 0
      org-edit-timestamp-down-means-later t
      org-startup-folded nil
      org-startup-align-all-tables t
      org-startup-with-inline-images nil
      org-startup-with-latex-preview t
      org-agenda-start-on-weekday nil
      org-agenda-span 14  ;; 14 days
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-export-odt-preferred-output-format "doc"
      org-tags-column 80
      org-startup-indented t)

(setq org-use-property-inheritance t)
(setq org-tag-alist '(("@urgent" . ?u) ("@normal" . ?n) 
                      ("trivial" . ?t)))

(setq org-export-html-postamble nil)

(setq-default org-structure-template-alist nil) ;; NO easy template, yasnippet instead

(setq org-export-html-style-include-default nil)
(setq org-export-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"/home/hongxuchen/.emacs.d/others/css/worg.css\" />") ;; 7.x
(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/home/hongxuchen/.emacs.d/others/css/worg.css\" />") ;; 8.x
; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))

(setq org-clock-persistence-insinuate t
      org-clock-persist t
      org-clock-in-resume t
      org-clock-in-switch-to-state "STARTED"
      org-clock-into-drawer t
      org-clock-out-remove-zero-time-clocks t)

;; Show iCal calendars in the org agenda
(when *is-a-mac*
  (eval-after-load "org"
    '(if *is-a-mac* (require 'org-mac-iCal)))
  (setq org-agenda-include-diary t)

  (setq org-agenda-custom-commands
        '(("I" "Import diary from iCal" agenda ""
           ((org-agenda-mode-hook
             (lambda ()
               (org-mac-iCal)))))))

  (add-hook 'org-agenda-cleanup-fancy-diary-hook
            (lambda ()
              (goto-char (point-min))
              (save-excursion
                (while (re-search-forward "^[a-z]" nil t)
                  (goto-char (match-beginning 0))
                  (insert "0:00-24:00 ")))
              (while (re-search-forward "^ [a-z]" nil t)
                (goto-char (match-beginning 0))
                (save-excursion
                  (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
                (insert (match-string 0))))))

(eval-after-load 'org
  '(progn
     (require 'htmlize)
     (require 'org-exp)
     (require 'org-clock)
     (setq org-src-fontify-natively t)
     (setq org-src-tab-acts-natively t)
     (defun soft-wrap-lines ()
       "Make lines wrap at window edge and on word boundary,
        in current buffer."
       (interactive)
       (setq word-wrap t)
       )
     (soft-wrap-lines)))

(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-org-cdlatex)
            (setq truncate-lines nil)))

;; http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

(evil-define-key 'normal org-mode-map
  "gh" 'outline-up-heading
  "gl" 'outline-next-visible-heading
  "$" 'org-end-of-line
  "^" 'org-beginning-of-line
  "-" 'org-ctrl-c-minus
  "<" 'org-promote-subtree
  ">" 'org-demote-subtree)

(setq org-agenda-files (list (expand-file-name  "~/.org/life.org") (expand-file-name  "~/.org/study.org") (expand-file-name "~/.org/other.org")))
;; (org-agena-list t)

(defun my-org-file-from-headline (file)
  (interactive
   (list
    (completing-read "File: "
                     (mapcar 'file-name-nondirectory
			     (file-expand-wildcards "~/.org/*.org"))
                     nil nil)))
  (unless (string-match "\\.org$" file)
    (error "Not an org file"))
  (save-excursion
    (beginning-of-line)
    (unless (org-at-heading-p)
      (error "Not on a headline")))
  (let* ((exists (file-exists-p file))
         (ftags (append
                 (list (file-name-sans-extension file))
                 (mapcar 'substring-no-properties org-file-tags)))
         (headline (nth 4 (org-heading-components)))
         (org-archive-reversed-order t)
         (org-archive-location (concat file "::"))
         (org-archive-save-context-info nil))
    (org-archive-subtree)
    (save-excursion (insert "* [[file:" file "][" file "]] - " headline "\n"))
    (find-file file)
    (goto-char (point-min))
    (save-excursion
      (if (re-search-forward "#\\+FILETAGS:\\(.*\\)$" nil t)
          (progn
            (save-match-data
              (setq ftags
                    (mapconcat 'identity
                               (org-uniquify
                                (append ftags
                                        (split-string
                                         (substring-no-properties
                                          (match-string 1))))) " ")))
            (replace-match (concat "#+FILETAGS: " ftags)))
        (insert "#+FILETAGS: " (mapconcat 'identity ftags " ") "\n"))
      (goto-char (point-min))
      (unless (re-search-forward "#\\+CATEGORY:\\(.*\\)$" nil t)
        (insert "#+CATEGORY: " (file-name-sans-extension file) "\n"))
      (goto-char (point-min))
      (when (re-search-forward "^Archived entries from file.+\n" nil t)
        (replace-match ""))))
  (write-file file))

(require 'ox-reveal)
(setq org-reveal-transition nil)
(setq org-reveal-root "file:///home/hongxuchen/.emacs.d/others/reveal.js/")

(defun org-other-modes ()
  (turn-on-orgtbl)
  (turn-on-orgstruct++))
;; nothing to do with org-mode though
(add-hook 'message-mode-hook 'org-other-modes)
(provide 'init-org)
