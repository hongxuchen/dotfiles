(provide 'init-autoloads)

;; simple-dict
(autoload 'dict-lookup-definition "simple-dict" "lookup words through DICT")

;; youdao-dict
(autoload 'youdao-dict "youdao-dict" "look up words via youdao dictionary")

;; llvm related
(setq auto-mode-alist (append '(("\\.ll$" . llvm-mode)) auto-mode-alist))
(autoload 'llvm-mode "llvm-mode" "major mode for ll files" t)
(setq auto-mode-alist (append '(("\\.td$" . tablegen-mode)) auto-mode-alist))
(autoload 'tablegen-mode "tablegen-mode" "major mode for tg files" t)

;; cc-mode related, doxymacs and cc-lookup
(autoload 'doxymacs-mode "doxymacs"
  "Minor mode for using/creating Doxygen documentation." t nil)
(autoload 'cc-lookup "cc-lookup" "load cc-lookup when needed" t)

;; file-template
(autoload 'file-template-find-file-not-found-hook "file-template" nil t)

;; escreen
(autoload 'escreen-create-screen "init-escreen" "make Emacs create Vim like tabs" t)
(evil-ex-define-cmd "tabe" 'escreen-create-screen)

;;douban fm
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
