(provide 'init-autoloads)

;; pretty-mode
(autoload 'turn-on-pretty-mode "pretty-mode")

;; simple-dict
(autoload 'dict-lookup-definition "simple-dict" "lookup words through DICT" t)

;; youdao-dict
(autoload 'youdao-dict "youdao-dict" "look up words via youdao dictionary" t)

;; llvm related
(setq auto-mode-alist (append '(("\\.ll$" . llvm-mode)) auto-mode-alist))
(autoload 'llvm-mode "llvm-mode" "major mode for ll files" t)
(setq auto-mode-alist (append '(("\\.td$" . tablegen-mode)) auto-mode-alist))
(autoload 'tablegen-mode "tablegen-mode" "major mode for tg files" t)
(add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/clang-format-3.4"))

;; cc-mode related, doxymacs and cc-lookup
(autoload 'doxymacs-mode "doxymacs"
  "Minor mode for using/creating Doxygen documentation." t nil)
(autoload 'cc-lookup "cc-lookup" "look up definitions in c/c++ mode" t)

;; file-template
(autoload 'file-template-find-file-not-found-hook "file-template" nil t)

;; escreen
(autoload 'escreen-create-screen "init-escreen" "make Emacs create Vim like tabs" t)
(evil-ex-define-cmd "tabe" 'escreen-create-screen)

;;douban fm
(setq douban-music-default-channel 10)
(autoload 'douban-music "douban-music-mode" nil t)

;; gnus
(autoload 'gnus "init-gnus" "the powerful gnu newsreader" t)
(autoload 'compose-mail "init-gnus" "compose mail using gnus" t)

;; google-translate
(autoload 'google-translate-at-point "google-translate" "google translate at point" t)
(autoload 'google-translate-query-translate "google-translate" "google translate" t)

;; info+
(eval-after-load "info" '(require 'info+))

;; apt-utils
(eval-after-load "apt-utils" '(require 'apt-utils-ido))

;; ffap
;; FIXME redefine rather than defadvice
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

;; eshell
(eval-after-load 'esh-opt '(progn (require 'init-eshell)))

;; smex
(eval-after-load 'smex '(defun smex-show-key-advice (command) ()))

;; oddmuse
;; Get around the emacswiki spam protection
(eval-after-load 'oddmuse
  (add-hook 'oddmuse-mode-hook
            (lambda ()
              (unless (string-match "question" oddmuse-post)
                (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post))))))

;;ffip
(eval-after-load "find-file-in-project"
  `(progn
     (setq ffip-limit 1024)
     (setq ffip-patterns (append ffip-patterns '("*.c" "*.c++" "*.cpp" "*.cc" "*.cxx" "*.h" "*.hpp" "Makefile")))
     (defun my-ffip-project-root-function ()
       "Check for `ffip-project-file' and if no such, \
return current directory."
       (let ((project-directory
              (if (listp ffip-project-file)
                  (some (apply-partially 'locate-dominating-file
                                         default-directory)
                        ffip-project-file)
                (locate-dominating-file default-directory
                                        ffip-project-file))))
         (or project-directory default-directory)))

     (setq-default
      ffip-project-root-function 'my-ffip-project-root-function)))
