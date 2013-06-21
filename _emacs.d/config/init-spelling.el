(dolist (hook '(message-mode-hook
                ))
  (add-hook hook 'flyspell-mode))
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

(setq ispell-personal-dictionary "~/.emacs.d/dict-spell/.aspell.en.pws")

(provide 'init-spelling)
