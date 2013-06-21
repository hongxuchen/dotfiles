(require 'eim)
(autoload 'eim-use-package "eim" "Another emacs input method")
(setq eim-punc-translate-p t)
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "pinyin" "EIM Chinese Pinyin Input Method" "py.txt"
 'my-eim-py-activate-function)
(setq eim-use-tooltip t)

(defun my-eim-py-activate-function ()
  (add-hook 'eim-active-hook
            (lambda ()
                (define-key eim-mode-map (kbd "M-o") 'eim-delete-last-char)
                (define-key eim-mode-map (kbd "M-c") 'eim-quit-clear)
                (define-key eim-mode-map (kbd "M-m") 'eim-quit-no-clear)
                (define-key eim-mode-map (kbd "M-p") 'eim-previous-page)
                (define-key eim-mode-map (kbd "M-n") 'eim-next-page)
                )))

;; (set-input-method "eim-py") ;; this setting is buggy
(setq default-input-method "eim-py")
(setq activate-input-method nil)
;; (toggle-input-method)

(remove-hook 'kill-emacs-hook 'eim-py-save-file)

(provide 'init-eim)
