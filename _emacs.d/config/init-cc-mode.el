(provide 'init-cc-mode)
(require 'rtags)
(rtags-enable-standard-keybindings c-mode-base-map)
(setq rtags-completion-enabled nil)
;; (setq rtags-path (expand-file-name "~/.bin/builds/rtags/bin"))

;; -----------------------------------------------------------------------------
;; some setups for cc-mode
;; -----------------------------------------------------------------------------
(setq cc-lookup-diagnostics-level 0)

(setq hide-ifdef-shadow t)

(progn
  (evil-define-key 'normal c++-mode-map "\C-]" 'irony-lookup)
  (evil-define-key 'normal c++-mode-map "\C-t" 'irony-jump-back)
  (evil-define-key 'normal c-mode-map "\C-]" 'irony-lookup)
  (evil-define-key 'normal c-mode-map "\C-t" 'irony-jump-back))

(add-hook 'c-mode-hook (lambda ()
                         (my-cc-mode-hook)
                         (local-set-key (kbd "<s-mouse-1>") 'irony-lookup)
                         (local-set-key (kbd "<s-mouse-3>") 'irony-jump-back)))

(add-hook 'c++-mode-hook (lambda ()
                           (my-cc-mode-hook)
                           (define-key c++-mode-map (kbd "<s-mouse-1>") 'irony-lookup)
                           (define-key c++-mode-map (kbd "<s-mouse-3>") 'irony-jump-back)))

(defun setup-cpputils ()
  "manually setup cpputils"
  (interactive)
  ;; FIXME
  (cppcm-reload-all)
  (remove-hook 'find-file-hook 'rinari-launch))

;; clang variable settings
(defun my-ac-cc-mode-setup ()

  ;; auto-complete-clang
  ;; (require 'auto-complete-clang)
  ;; (setq ac-clang-executable "/usr/bin/clang")
  ;; (set (make-local-variable 'ac-auto-start) nil)

  ;; irony-mode
  (require 'irony)
  (require 'irony-chx)
  (irony-mode 1)
  (irony-enable 'ac)
  ;; (irony-enable 'flycheck)
  (setq ac-sources '(ac-source-irony ac-source-yasnippet ac-source-dictionary))
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
  (setq comment-start "/// " comment-end "")
  (doxymacs-mode t)
  (doxymacs-font-lock)
  ;; (require 'cpp)
  ;; (global-cwarn-mode t)
  ;; (hide-ifdef-mode t)
  ;; (default-cc-flags-setup)
  (my-ac-cc-mode-setup))

;; -----------------------------------------------------------------------------
;; includes and flags
;; -----------------------------------------------------------------------------

(add-hook 'cc-mode-hook 'turn-on-auto-fill)

(dir-locals-set-class-variables 'llvm-3.4-directory
                                '((nil . ((irony-compile-flags. ("-I/usr/lib/llvm-3.4/include"   "/usr/include/c++/4.6" "/usr/include/c++/4.6/backward" "/usr/include/c++/4.6/x86_64-linux-gnu" "/usr/include/c++/4.6/i686-linux-gnu" "/usr/lib/gcc/x86_64-linux-gnu/4.6/include"))))
                                  (nil . ((irony-compile-flags . ("/usr/lib/llvm-3.4/include" "/usr/include" "/usr/include/linux" "/usr/local/include" "/usr/include/c++/4.6/"))))))
(dir-locals-set-directory-class
 "/usr/lib/llvm-3.4/include/clang-c/" 'llvm-3.4-directory)

(dir-locals-set-directory-class
 "/usr/lib/llvm-3.4/include/clang" 'llvm-3.4-directory)
