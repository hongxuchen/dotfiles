(add-auto-mode 'ruby-mode "\\.rb\\'" "Rakefile\\'" "\.rake\\'" "\.rxml\\'" "\.rjs\\'" ".irbrc\\'" "\.builder\\'" "\.ru\\'" "\.gemspec\\'" "Gemfile\\'" "\\.ru\\" "\\.Capfile" "\\.Vagrantfile\\")

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(setq ruby-use-encoding-map nil)

(defun my-ruby-compilation-keys ()
  (local-set-key [S-f7] 'ruby-compilation-this-buffer)
  (local-set-key [f7] 'ruby-compilation-this-test)
  (local-set-key [f6] 'recompile)
  )
(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)
     (my-ruby-compilation-keys)
     (setq compile-command "rake ")))

(add-hook 'robe-mode-hook
          (lambda ()
            (flymake-ruby-load)
            (robe-mode)
            (add-to-list 'ac-sources 'ac-source-robe)
            (setq completion-at-point-functions '(auto-complete))))

(defalias 'ri 'yari)

(dolist (hook (list 'html-mode-hook 'nxml-mode-hook 'yaml-mode-hook))
  (add-hook hook '(require 'mmm-erb)))

(dolist (mode (list 'html-mode 'html-erb-mode 'nxml-mode))
  (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
  (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)
  (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))

(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

(add-to-list 'auto-mode-alist '("\\.r?html\\(\\.erb\\)?\\'" . html-erb-mode))
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))
(mmm-add-mode-ext-class 'yaml-mode "\\.yaml\\'" 'erb)

(dolist (mode (list 'js-mode 'js2-mode 'js3-mode))
  (mmm-add-mode-ext-class mode "\\.js\\.erb\\'" 'erb))

;;----------------------------------------------------------------------------
;; Ruby - my convention for heredocs containing SQL
;;----------------------------------------------------------------------------
;; (eval-after-load 'mmm-mode
;;   '(progn
;;      (mmm-add-classes
;;       '((ruby-heredoc-sql :submode sql-mode :front "<<-?end_sql.*\r?\n" :back "[ \t]*end_sql" :face mmm-code-submode-face)))
;;      (mmm-add-mode-ext-class 'ruby-mode "\\.rb\\'" 'ruby-heredoc-sql)))

;;----------------------------------------------------------------------------
;; yari
; You can use C-u M-x yari to reload all completion targets.
(require 'yari)
(defun ri-bind-key ()
  (local-set-key "\C-c;y" 'yari-anything))
(add-hook 'ruby-mode-hook 'ri-bind-key)
(add-hook 'nxml-mode-hook 'ri-bind-key)
(add-hook 'html-mode-hook 'ri-bind-key)
(add-hook 'inf-ruby-mode-hook 'ri-bind-key)

;;; rails
(global-rinari-mode)

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command (concat "ctags -a -e -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))


(provide 'init-ruby)
