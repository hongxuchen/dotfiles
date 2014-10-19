;; ------------------------------------------------------------------------------
;; navigating issues
;; ------------------------------------------------------------------------------
(defsubst navigable-symbols ()
  "Return a list of strings for the symbols to which navigation is possible."
  (cl-loop for x being the symbols
           if (or (fboundp x) (boundp x) (symbol-plist x) (facep x))
           collect (symbol-name x)))

(defsubst read-symbol-at-point ()
  "Return the symbol at point as a string.
If `current-prefix-arg' is not nil, the user is prompted for the symbol."
  (let* ((sym-at-point (symbol-at-point))
         (at-point (and sym-at-point (symbol-name sym-at-point))))
    (if current-prefix-arg
        (completing-read "Symbol: "
                         (navigable-symbols)
                         nil t at-point)
      at-point)))

(defun find-elisp-thing-at-point (sym-name)
  "Jump to the elisp thing at point, be it a function, variable, library or face.
With a prefix arg, prompt for the symbol to jump to.
Argument SYM-NAME thing to find."
  (interactive (list (read-symbol-at-point)))
  (when sym-name
    (let ((sym (intern sym-name)))
      (cond
       ((fboundp sym) (find-function sym))
       ((boundp sym) (find-variable sym))
       ((or (featurep sym) (locate-library sym-name))
        (find-library sym-name))
       ((facep sym)
        (find-face-definition sym))
       (t (error "Don't know how to find <%s>" sym))))))

;; ------------------------------------------------------------------------------
;; paredit http://mumble.net/~campbell/emacs/paredit.html
;; ------------------------------------------------------------------------------
(autoload 'enable-paredit-mode "paredit")

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-hook 'paredit-mode-hook (lambda ()
                               (unless (or (eq major-mode 'inferior-emacs-lisp-mode) (minibufferp))
                                 (local-set-key (kbd "RET") 'paredit-newline)
                                 (local-set-key (kbd "C-k") 'paredit-kill))
                               (local-set-key (kbd "C-x C-a") 'pp-macroexpand-last-sexp)
                               (evil-surround-mode -1)))

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'paredit-mode)

;; -----------------------------------------------------------------------------
;; elisp modes
;; -----------------------------------------------------------------------------

;; Prevent flickery behaviour due to hl-sexp-mode unhighlighting before each command
(eval-after-load 'hl-sexp
  '(defadvice hl-sexp-mode (after unflicker (turn-on) activate)
     (when turn-on
       (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

(defun my-elisp-mode-setup ()
  ;; (hl-sexp-mode 1)
  (paredit-mode 1)
  (eldoc-mode 1)
  (yas-minor-mode -1)
  (my-elisp-mode-ac-setup)
  )

(if (fboundp 'evil-mode)
    (progn
      (evil-define-key 'normal emacs-lisp-mode-map "gd" 'find-elisp-thing-at-point)
      (evil-define-key 'normal ielm-map "gd" 'find-elisp-thing-at-point)
      ))

(fset 'inferior-lisp-mode 'emacs-lisp-mode)
(fset 'lisp-interaction-mode 'emacs-lisp-mode)

(let ((elispy-hooks
       '(emacs-lisp-mode-hook
         ielm-mode-hook
         ))
      )
  (dolist (hook elispy-hooks)
    (add-hook hook 'my-elisp-mode-setup))
  )

(defun my-byte-compile-on-save ()
  (add-hook 'after-save-hook
            (lambda ()
              (when
                  (and
                   (buffer-file-name)
                   (not (member (file-name-nondirectory (buffer-file-name)) '(".dir-locals.el" "init-elpa.el")))
                   (string= major-mode 'emacs-lisp-mode))
                (and (file-exists-p (concat buffer-file-name "c")) (byte-compile-file buffer-file-name))
                (eval-buffer)))
            t))
(add-hook 'emacs-lisp-mode-hook 'my-byte-compile-on-save)

(setq source-directory (expand-file-name "~/.bin/builds/emacs"))


(provide 'init-lisp)
