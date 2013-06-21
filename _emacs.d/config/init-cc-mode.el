(provide 'init-cc-mode)

;; -----------------------------------------------------------------------------
;; some setups for cc-mode
;; -----------------------------------------------------------------------------

(defun setup-cc-keymaps()
  (evil-define-key 'normal c++-mode-map "\C-]" 'cc-lookup)
  (evil-define-key 'normal c-mode-map "\C-]" 'cc-lookup))

(defun setup-cpputils ()
  "manually setup cpputils"
  (interactive)
  ;; FIXME
  (setq cc-search-directories my-c++-include-directories)
  (cppcm-reload-all)
  (remove-hook 'find-file-hook 'rinari-launch))

;; clang variable settings
;; @see https://github.com/brianjcj/auto-complete-clang
(defun my-ac-cc-mode-setup ()
  (require 'auto-complete-clang)
  (setq ac-clang-executable "/usr/bin/clang")
  (setq ac-sources '(ac-source-clang ac-source-yasnippet ac-source-dictionary))
  (setq ac-clang-auto-save t)

  ;; (require 'auto-complete-clang-async)
  ;; (setq ac-clang-complete-executable "~/.emacs.d/site-lisp/emacs-clang-complete-async/clang-complete")
  ;; (setq ac-sources '(ac-source-clang-async))
  ;; (ac-clang-launch-completion-process)
  )

;; -----------------------------------------------------------------------------
;; customize my hooks
;; -----------------------------------------------------------------------------
(defun my-cc-mode-hook ()
  (setq compilation-window-height 8)

  (setq c-style-variables-are-local-p nil)
  ;; NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)
  ;; make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)
  ;; do not impose restriction that all lines not top-level be indented at least 1
  (setq c-label-minimum-indentation 0)
  (require 'google-c-style)
  (google-set-c-style)
  (c-set-style "Google")
  (setq c-default-style "Google")
  (setq comment-start "// " comment-end "")
  (doxymacs-mode t)
  (require 'fic-mode)
  (turn-on-fic-mode)
  (require 'which-func)
  (which-function-mode t)
  (hide-ifdef-mode t)
  (default-cc-flags-setup)
  (my-ac-cc-mode-setup)
  )

(add-hook 'c-mode-hook 'my-cc-mode-hook)
(add-hook 'c++-mode-hook 'my-cc-mode-hook)
(add-hook 'c-initialization-hook' setup-cc-keymaps)


;REVIEW 2013-06-14 10:46 Hongxu Chen;
;;cwarn

;; -----------------------------------------------------------------------------
;; includes and flags
;; -----------------------------------------------------------------------------

;; setup all cc flags values
;; FIXME shouldn't be used interactively
(defun default-cc-flags-setup ()
  "setup all of my paths and flags in c/c++ mode."
  (setq my-c++-include-directories
        (split-string "
  /usr/include/c++/4.6
  /usr/include/c++/4.6/backward
  /usr/include/c++/4.6/x86_64-linux-gnu
  /usr/include/c++/4.6/i686-linux-gnu
  /usr/lib/gcc/x86_64-linux-gnu/4.6/include
  /usr/lib/gcc/i686-linux-gnu/4.6/include
  /usr/lib/gcc/i686-linux-gnu/4.6/include-fixed
  /usr/include/i386-linux-gnu
  /usr/local/include
  /usr/include/x86_64-linux-gnu
  /usr/include
                    .
                    "))

  (setq my-ac-extra-flags
        '("-I/usr/local/include" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include"))


  (setq ffap-c-path '("/usr/include" "/usr/include/linux" "/usr/local/include"))

  (setq my-c++-include-flags
        (mapcar (lambda (item)(concat "-I" item))
                my-c++-include-directories))

  (if (eq major-mode 'c++-mode)
      (setq ac-clang-flags (append my-c++-include-flags '("-std=c++11")))
      (setq ac-clang-flags my-c++-include-flags))
  )

(defun emacs-format-function ()
   "Format the whole buffer."
   (interactive)
   (c-set-style "Google")
   (indent-region (point-min) (point-max) nil)
   (untabify (point-min) (point-max))
   (save-buffer))
