(provide 'init-gnus)

(require 'gnus-cite)
(require 'gnus-setup)

;;-----------------------------------------------------------------------------
;; gnus basic settings
;;-----------------------------------------------------------------------------

;; (setq gnus-agent-expire-all nil);;default
;; (setq gnus-activate-level 2)
;; (setq gnus-read-active-file nil)
;; (setq gnus-visual t)

;; (add-hook 'gnus-article-mode-hook 'linum-on)
(setq gnus-asynchronous t)
(setq gnus-use-cache t)
(setq gnus-verbose 1)
(setq gnus-save-newsrc-file nil)
(setq gnus-read-newsrc-file nil)
(setq gnus-startup-file (expand-file-name "~/.emacs.d/.newsrc"))
(setq
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 ;; gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 )
(setq gnus-suppress-duplicates t)
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; signature settings
(setq gnus-posting-styles
      '((".*" (address "leftcopy.chx@gmail.com")
         (signature-file "~/.emacs.d/signature"))))
(setq gnus-treat-hide-signature t)
(setq gnus-inhibit-startup-message t)
(setq gnus-auto-select-next 'almost-quietly)

(remove-hook 'gnus-mark-article-hook
             'gnus-summary-mark-read-and-unread-as-read)
(add-hook 'gnus-mark-article-hook 'gnus-summary-mark-unread-as-read)

(setq
 gnus-use-trees t                                                       ;联系老的标题
 gnus-tree-minimize-window nil                                          ;用最小窗口显示
 ;; gnus-fetch-old-headers 'some                                           ;抓取老的标题以联系线程,速度极慢
 gnus-generate-tree-function 'gnus-generate-horizontal-tree             ;生成水平树
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject ;聚集函数根据标题聚集
 )

(setq gnus-fetch-old-headers t)

(setq rfc2047-header-encoding-alist
      '(("Newsgroups" . t)
        ("Message-ID" . nil)
        ("Subject" . mime)
        ("From" . default)
        ("To" . default)
        (t . mime)))

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(setq mm-text-html-renderer 'shr)
(setq mm-default-directory "~/")
(setq mm-inline-text-html-with-images t)
(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)
(setq mm-w3m-safe-url-regexp nil)
;; (setq mml-smime-signers (quote ("27E94A1A")))
(setq display-time-use-mail-icon t)

;; default
;; (setq starttls-use-gnutls t)
;; (setq starttls-gnutls-program "gnutls-cli")
;; (setq starttls-extra-arguments nil)

(setq gnus-use-sc t)
;; (setq gnus-use-rmail t)
;; (setq gnus-use-vm t)

(setq gnus-large-newsgroup 200) ;; default 200
;; (setq gnus-check-new-newgroups nil)
(setq gnus-summary-display-arrow t
      gnus-treat-display-smileys nil ; I do not like graphics smileys.
      gnus-keep-backlog 50 ;default 20
      gnus-auto-center-summary nil
      gnus-auto-select-next nil)

;;-----------------------------------------------------------------------------
;; receive email,use fetchmail+procmail to put new mail into ~/Mail
;;-----------------------------------------------------------------------------
(setq
 gnus-select-method '(nnmaildir "mymailbox" (directory "~/Mail/"))
 mail-sources '((maildir :path "~/Mail/" :subdirs ("cur" "new")))
 mail-source-delete-incoming t
 )
;;; A simple settings without dependence of external programs, but might be slow
;; (setq gnus-select-method
;;       '(nnimap "gmail"
;;                (nnimap-address "imap.gmail.com")
;;               (nnimap-server-port 993)
;;                (nnimap-stream ssl)))

;;-----------------------------------------------------------------------------
;; outbox settings
;;-----------------------------------------------------------------------------
(eval-after-load "message"
  '(progn
     (require 'org-contacts)
     (add-to-list 'org-capture-templates
                  '("c" "Contacts" entry (file "~/.org/contacts.org")
                    "* %(org-contacts-template-name)
                  :PROPERTIES:
                  :EMAIL: %(org-contacts-template-email)
                  :END:"))
     (setq org-contacts-files '("~/.org/contacts.org"))
     ))

(setq gnus-gcc-mark-as-read t) ;; don't re-read
(setq gnus-message-archive-group '("nnmaildir+mymailbox:sent," "nnmaildir+mymailbox:inbox"))
;; smtp to send email directly(no need for other external programs)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "leftcopy.chx@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "STAP")

;; (setq message-send-mail-function 'message-send-mail-with-sendmail
;;       sendmail-program "/usr/bin/msmtp")

(if window-system
    (setq
     gnus-sum-thread-tree-root "● "
     gnus-sum-thread-tree-false-root "▷ "
     gnus-sum-thread-tree-single-indent ""
     gnus-sum-thread-tree-leaf-with-other "├─►"
     gnus-sum-thread-tree-vertical "│ "
     gnus-sum-thread-tree-single-leaf "└─►")
  (setq
   gnus-sum-thread-tree-false-root ""
   gnus-sum-thread-tree-indent ""
   gnus-sum-thread-tree-leaf-with-other "-> "
   gnus-sum-thread-tree-root ""
   gnus-sum-thread-tree-single-leaf "|_ "
   gnus-sum-thread-tree-vertical "|"
   ))

;; Automate the fetching of mail.
;; (require 'gnus-demon)
;; Check for new mail once in every this many minutes.
(gnus-demon-add-handler 'gnus-demon-scan-news 5 nil)

(setq gnus-interactive-exit nil)
(setq gnus-inhibit-startup-message t)

;; Sorting
(setq gnus-article-sort-functions '(gnus-article-sort-by-number
                                    gnus-article-sort-by-subject
                                    gnus-article-sort-by-date))

(setq gnus-thread-sort-functions '(gnus-thread-sort-by-number
                                   gnus-thread-sort-by-subject
                                   gnus-thread-sort-by-total-score
                                   gnus-thread-sort-by-date))
(setq message-from-style 'angles)
(setq message-from-style 'default)
(setq message-kill-buffer-on-exit t)
(setq message-syntax-checks '((sender . disabled) (from . disabled)))

(add-hook 'gnus-summary-mode-hook 'turn-on-gnus-mailing-list-mode)
(setq gnus-boring-article-headers (list 'long-to))

;; (defadvice save-buffers-kill-emacs (before quit-gnus (&rest args) activate)
;;   (let (buf)
;;     (when (and (fboundp 'gnus-alive-p)
;;                (gnus-alive-p)
;;                (bufferp (setq buf (get-buffer "*Group*"))))
;;       (with-current-buffer buf
;;         (gnus-group-exit)))))

(setq message-generate-headers-first nil)

(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 25
                         (group 1.0))
               (vertical 1.0
                         (summary 1.0 point)))))

(defun my-hide-show-thread ()
  "If the current thread is folded then unfold it, vice versa"
  (interactive)
  (let ((first-children (car (gnus-summary-article-children))))
    (if first-children
        (progn
          (next-line)
          ;; check whether current line is the folded last line, if so, unfold it
          (if (string= (what-line) (save-excursion (end-of-buffer) (what-line)))
              (progn
                (previous-line)
                (gnus-summary-show-thread))
            (if (eq (gnus-summary-article-number) first-children)
                (progn
                  (previous-line)
                  (gnus-summary-hide-thread))
              (progn
                (previous-line)
                (gnus-summary-show-thread)))))
      (message "no subtree here"))))

(setq gnus-summary-mode-hook
      '(lambda ()
         (local-unset-key [(tab)])
         (local-set-key [(tab)] 'my-hide-show-thread)))
