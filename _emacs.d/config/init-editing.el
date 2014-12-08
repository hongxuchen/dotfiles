(setq-default blink-cursor-delay 0
              fill-column 80
              blink-cursor-interval 0.4
              bookmark-default-file "~/.emacs.d/.bookmarks.el"
              buffers-menu-max-size 20
              regex-tool-backend 'perl
              case-fold-search t
              lazy-highlight-cleanup nil
              case-replace nil
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              max-mini-window-height 1
              indent-tabs-mode nil
              line-spacing 0.2
              set-mark-command-repeat-pop t
              truncate-lines nil
              disabled-command-function nil
              echo-keystrokes 0.1
              Info-use-header-line t
              isearch-allow-scroll t
              completion-show-help nil
              kill-whole-line t
              help-window-select t
              truncate-partial-width-windows nil
              visible-bell nil
              indicate-empty-lines nil
              indicate-buffer-boundaries nil
              global-auto-revert-non-file-buffers t
              revert-without-query t
              auto-revert-verbose nil
              list-directory-brief-switches (purecopy "-ACF")
              find-file-suppress-same-file-warnings t
              view-read-only t
              auto-save-default nil)

(transient-mark-mode 1)
(delete-selection-mode 1)
(global-pointback-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(autopair-global-mode 1)
(setq show-paren-delay 0.00
      autopair-autowrap t)

;; (tooltip-mode -1)
;; (global-whitespace-mode -1)
;; (setq whitespace-style '(face empty tabs lines-tail trailing))

;; grep
(setq grep-program "grep"
      grep-command "grep -inH"
      grep-highlight-matches t
      grep-scroll-output t)
(setq wgrep-auto-save-buffer t)
(eval-after-load "rgrep" '(require 'wgrep))
(eval-after-load "lgrep" '(require 'wgrep))
(eval-after-load "grep" '(require 'wgrep))
(setq ag-reuse-buffers t
      ag-reuse-window t)

(setq grep-files-aliases
      '(("all" . "* .*")
        ("*.l" . "*.l *.y *.[ch] *.hpp *.cc *.cpp")
        ("*.y" . "*.l *.y *.[ch] *.hpp *.cc *.cpp")
        ("el" . "*.el")
        ("ch" . "*.[ch] *.hpp *.cc *.cxx *.cpp *.C *.CC *.c++")
        ("c" . "*.c")
        ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++ *.c *.h *.hpp")
        ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
        ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
        ("h" . "*.h")
        ("l" . "[Cc]hange[Ll]og*")
        ("m" . "[Mm]akefile*")
        ("tex" . "*.tex")
        ("texi" . "*.texi")
        ("asm" . "*.[sS]")
        ))

(defun my-prog-mode-basic-setup ()
  (require 'fic-mode)
  (require 'which-func)
  (fic-mode 1)
  (which-function-mode 1)
  (rainbow-mode 1)
  (rainbow-delimiters-mode 1)
  (visual-line-mode 1)
  (hs-minor-mode 1)
  )
(add-hook 'prog-mode-hook 'my-prog-mode-basic-setup)
(setq flymake-gui-warnings-enabled nil)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; spelling
(dolist (hook
         '(message-mode-hook
           org-mode-hook
           ))
  (add-hook hook 'flyspell-mode))
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-personal-dictionary "~/.emacs.d/dict-spell/.aspell.en.pws"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
        ispell-quietly t
        ispell-silently-savep t
        ))

;; kill/yank/paste
(if (display-graphic-p)
    (setq x-select-enable-clipboard t
          x-select-enable-primary t
          select-active-regions nil
          mouse-drag-copy-region t
          kill-do-not-save-duplicates t
          mouse-yank-at-point t)
  (setq x-select-enable-clipboard nil
        x-select-enable-clipboard-manager nil
        x-select-enable-primary nil))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; file content
(setq backup-by-coping t ; don't clobber symlinks
      backup-directory-alist `(("." . ,temporary-file-directory))
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t
      vc-make-backup-files nil)

;; nicer naming
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; ffap
;; FIXME it's a redefine rather than defadvice
(eval-after-load "ffap"
  '(defun ffap-read-file-or-url (prompt guess)
     "Read file or URL from minibuffer, with PROMPT and initial GUESS."
     (or guess (setq guess default-directory))
     (let (dir)
       (or (ffap-url-p guess)
           (progn
             (or (ffap-file-remote-p guess)
                 (setq guess
                       (abbreviate-file-name (expand-file-name guess))
                       ))
             (setq dir (file-name-directory guess))))
       ;; Do file substitution like (interactive "F"), suggested by MCOOK.
       (or (ffap-url-p guess) (setq guess (substitute-in-file-name guess)))
       ;; Should not do it on url's, where $ is a common (VMS?) character.
       ;; Note: upcoming url.el package ought to handle this automatically.
       guess)))

;;ffip
(eval-after-load "find-file-in-project"
  `(progn
     (setq ffip-limit 1024)
     (setq ffip-patterns
           (append ffip-patterns
                   '("*.c" "*.c++" "*.cpp" "*.cc" "*.cxx"
                     "*.h" "*.hpp"
                     "Makefile"
                     )))
     (defun my-ffip-project-root-function ()
       "Check for `ffip-project-file' and if no such, \
return current directory."
       (require 'ffip)
       (let ((project-directory
              (if (listp ffip-project-file)
                  (some (apply-partially 'locate-dominating-file
                                         default-directory)
                        ffip-project-file)
                (locate-dominating-file default-directory
                                        ffip-project-file))))
         (or project-directory default-directory)))
     (setq-default
      ffip-project-root-function 'my-ffip-project-root-function
      )))

(provide 'init-editing)
