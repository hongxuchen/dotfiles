(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point nil
      ido-ubiquitous-enable-compatibility nil
      ido-ubiquitous-enable-old-style-default nil
      ido-auto-merge-work-directories-length -1
      ido-enable-regexp nil
      ido-max-prospects 12
      ido-max-window-height 2
      ido-use-virtual-buffers t
      ido-create-new-buffer 'always
      ido-save-directory-list-file "~/.emacs.d/.ido.last"
      ido-use-faces t
      ;; allow same buffer opened in different frames
      ido-default-buffer-method 'selected-window
      ido-ignore-directories (append ido-ignore-directories '("^auto/$" "\\.prv/" "_region_"))
      ido-ignore-files (append ido-ignore-files '("^auto/$" "_region_"))
      ido-ignore-buffers (append ido-ignore-buffers '("*Messages*" "*scratch*" "*IBuffer*"))
      )

(smex-initialize)

(setq imenu-use-popup-menu t
      imenu-eager-completion-buffer nil
      imenu-auto-rescan t)

(setq echo-keystrokes 0)

(defvar paredit-minibuffer-commands
  '(eval-expression
    pp-eval-expression
    eval-expression-with-eldoc
    ibuffer-do-eval
    ibuffer-do-view-and-eval))
(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(define-key minibuffer-local-map [escape] 'keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map (kbd "<tab>") 'minibuffer-complete)

(provide 'init-minibuffer)
