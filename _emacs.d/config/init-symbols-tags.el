;; taglist
(autoload 'taglist "taglist" "a Vim like taglist using speedbar/imenu" t nil)

(setq speedbar-frame-parameters
      '((minibuffer)
        (width . 20)
        (border-width . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (unsplittable . t)
        (left-fringe . 0)))
(setq speedbar-hide-button-brackets-flag t
      speedbar-verbosity-level 0
      speedbar-use-images nil
      speedbar-show-unknown-files t
      dframe-update-speed nil)

(add-hook 'speedbar-reconfigure-keymaps-hook
          '(lambda ()
             (define-key speedbar-key-map (kbd "^") 'speedbar-up-directory)
             (define-key speedbar-key-map (kbd "o")
               '(lambda ()
                  (speedbar-edit-line)
                  (other-window 1)))))

(setq speedbar-tag-hierarchy-method nil)
(setq speedbar-default-position 'left)
(setq speedbar-verbosity-level 2)

;; (eval-after-load 'speedbar
;;   '(progn
;;      (require 'semantic/sb)
;;      (semantic-mode)
;;      ))

(provide 'init-symbols-tags)
