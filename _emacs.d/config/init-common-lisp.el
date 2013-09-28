;; See http://bc.tech.coop/blog/070927.html
(add-auto-mode 'lisp-mode "\\.cl\\'")
(add-hook 'lisp-mode-hook (lambda ()
                            (cond ((not (featurep 'slime))
                                   (require 'slime)
                                   (normal-mode)))))

;; From http://bc.tech.coop/blog/070515.html
(defun lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol currently under the curser"
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

(define-key lisp-mode-map (kbd "C-c l") 'lispdoc)

;;; slime

(autoload 'slime-fuzzy-init "slime-fuzzy" "" nil)
;; use sbcl, `C-h v slime-read-interactive-args RET` for details
(setq slime-default-lisp 'sbcl)
(eval-after-load 'slime-fuzzy
  '(require 'slime-repl))

(defun smp/set-up-slime-repl-auto-complete ()
  "Bind TAB to `indent-for-tab-command', as in regular Slime buffers."
  (local-set-key (kbd "TAB") 'indent-for-tab-command))

(eval-after-load 'slime
  '(progn
     (add-to-list 'slime-lisp-implementations
                  '(sbcl ("sbcl") :coding-system utf-8-unix))
     (add-to-list 'slime-lisp-implementations
                  '(cmucl ("lisp") :coding-system iso-latin-1-unix))
     (add-to-list 'load-path (concat (directory-of-library "slime") "/contrib"))
     (setq slime-protocol-version 'ignore)
     (setq slime-net-coding-system 'utf-8-unix)
     (add-hook 'slime-repl-mode-hook 'sanityinc/lisp-setup)
     (slime-setup '(slime-repl slime-fuzzy))
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     ;; Stop SLIME's REPL from grabbing DEL, annoying when backspacing over a '('
     (defun override-slime-repl-bindings-with-paredit ()
       (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))
     (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
     (add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand)
     (add-hook 'slime-repl-mode-hook (lambda () (setq show-trailing-whitespace nil)))
     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'smp/set-up-slime-repl-auto-complete)
     (eval-after-load 'auto-complete
       '(add-to-list 'ac-modes 'slime-repl-mode))))

(provide 'init-common-lisp)
